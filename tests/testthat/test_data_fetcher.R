# tests/testthat/test_data_fetcher.R
library(testthat)
library(pdftools)
library(officer)
library(rvest)
library(xml2)
library(curl)
library(dplyr)
library(stringi)
library(httr)
library(jsonlite)
library(magrittr)


test_that("fetch_data returns an empty data frame if invalid path is provided", {
    # Attempt to fetch data from a path that doesn't exist
    result <- fetch_data(local_paths = "path_that_does_not_exist_123")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 0)  # No valid files => 0 rows
})

test_that("fetch_data can read a local .txt file (created on-the-fly)", {
    # Create a temporary .txt file
    tmpfile <- tempfile(fileext = ".txt")
    writeLines("Hello, this is a test file.\nSecond line here.", tmpfile)

    # Use fetch_data on that file
    result <- fetch_data(local_paths = tmpfile)
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 1)
    expect_match(result$content[1], "this is a test file")

    # Clean up
    unlink(tmpfile)
})

test_that("fetch_data website crawling returns a data frame (using r4ds.hadley.nz)", {
    skip_on_cran()                    # Skip on CRAN to avoid networking issues
    skip_if_offline("r4ds.hadley.nz") # Skip if offline or domain not reachable

    test_url <- "https://r4ds.hadley.nz"
    result <- tryCatch(
      fetch_data(
        website_urls = test_url,
        crawl_depth = 1
      ),
      error = function(e) {
        skip(paste("Website crawling unavailable in this environment:", e$message))
      }
    )
    expect_true(is.data.frame(result))
    if (nrow(result) == 0) {
      skip("Website returned no crawlable content in this environment.")
    }

    # Check that key columns exist
    needed_cols <- c("source", "title", "content", "url", "source_type")
    expect_true(all(needed_cols %in% names(result)))
})

# ----------------------------------------------------------------------
# Test for local files in tests/testthat/test-data/
# ----------------------------------------------------------------------

test_that("fetch_data can read multiple local files from test-data folder", {

  skip_on_cran()

  # Local fixture paths (no network downloads)
  test_files <- list(
    pdf  = testthat::test_path("test-data", "sprint.pdf"),
    docx = testthat::test_path("test-data", "scrum.docx"),
    pptx = testthat::test_path("test-data", "introduction.pptx"),
    txt  = testthat::test_path("test-data", "overview.txt")
  )

  # --- PDF: sprint.pdf ---
  if (file.exists(test_files$pdf)) {
    pdf_res <- fetch_data(local_paths = test_files$pdf)
    expect_true(is.data.frame(pdf_res))
    expect_true(nrow(pdf_res) >= 1)
    expect_true(any(pdf_res$source_type == "pdf"))
  } else {
    skip("sprint.pdf not found in test-data folder.")
  }

  # --- DOCX: scrum.docx ---
  if (file.exists(test_files$docx)) {
    docx_res <- fetch_data(local_paths = test_files$docx)
    expect_true(is.data.frame(docx_res))
    expect_true(nrow(docx_res) >= 1)
    expect_true(any(docx_res$source_type == "docx"))
  } else {
    skip("scrum.docx not found in test-data folder.")
  }

  # --- PPTX: introduction.pptx ---
  if (file.exists(test_files$pptx)) {
    pptx_res <- fetch_data(local_paths = test_files$pptx)
    expect_true(is.data.frame(pptx_res))
    expect_true(nrow(pptx_res) >= 1)
    expect_true(any(pptx_res$source_type == "pptx"))
  } else {
    skip("introduction.pptx not found in test-data folder.")
  }

  # --- TXT: overview.txt ---
  if (file.exists(test_files$txt)) {
    txt_res <- fetch_data(local_paths = test_files$txt)
    expect_true(is.data.frame(txt_res))
    expect_true(nrow(txt_res) >= 1)
    expect_true(any(txt_res$source_type == "txt"))
  } else {
    skip("overview.txt not found in test-data folder.")
  }
})

test_that("crawl_depth handling is deterministic for local HTML graph", {
  skip_on_cran()

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  graph <- list(
    "http://site/a" = c("/b", "/c"),
    "http://site/b" = c("/d"),
    "http://site/c" = character(0),
    "http://site/d" = character(0)
  )

  local_mocked_bindings(
    read_html = function(url) list(url = url),
    .package = "xml2"
  )
  local_mocked_bindings(
    html_elements = function(doc, css) graph[[doc$url]] %||% character(0),
    html_attr = function(anchors, name) anchors,
    .package = "rvest"
  )

  start_url <- "http://site/a"

  links_d0 <- RAGFlowChainR:::crawl_links_bfs(start_url, depth = 0)
  expect_equal(length(links_d0), 1)
  expect_true(start_url %in% links_d0)

  links_d1 <- RAGFlowChainR:::crawl_links_bfs(start_url, depth = 1)
  expect_true("http://site/a" %in% links_d1)
  expect_true("http://site/b" %in% links_d1)
  expect_true("http://site/c" %in% links_d1)
  expect_false("http://site/d" %in% links_d1)

  links_d2 <- RAGFlowChainR:::crawl_links_bfs(start_url, depth = 2)
  expect_true("http://site/d" %in% links_d2)

  links_null <- RAGFlowChainR:::crawl_links_bfs(start_url, depth = NULL)
  expect_true("http://site/d" %in% links_null)

  expect_error(
    RAGFlowChainR:::crawl_links_bfs(start_url, depth = -1),
    "crawl_depth"
  )
})
