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
    result <- fetch_data(
        website_urls = test_url,
        crawl_depth = 1
    )
    expect_true(is.data.frame(result))
    expect_gt(nrow(result), 0)

    # Check that key columns exist
    needed_cols <- c("source", "title", "content", "url", "source_type")
    expect_true(all(needed_cols %in% names(result)))
})

# ----------------------------------------------------------------------
# Test for local files in tests/testthat/test-data/
# ----------------------------------------------------------------------

# Helper function to download a file if not present (skip on CRAN)
download_test_file_if_needed <- function(local_path, github_url) {
  if (!file.exists(local_path)) {
    message(sprintf("Downloading %s from GitHub...", basename(local_path)))
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
    download.file(github_url, destfile = local_path, mode = "wb", quiet = TRUE)
  }
}

test_that("fetch_data can read multiple local files from test-data folder", {

  skip_on_cran()

  # Define local paths and GitHub URLs
  test_files <- list(
    pdf  = list(
      path = testthat::test_path("test-data", "sprint.pdf"),
      url  = "https://github.com/knowusuboaky/RAGFlowChainR/raw/main/tests/testthat/test-data/sprint.pdf"
    ),
    docx = list(
      path = testthat::test_path("test-data", "scrum.docx"),
      url  = "https://github.com/knowusuboaky/RAGFlowChainR/raw/main/tests/testthat/test-data/scrum.docx"
    ),
    pptx = list(
      path = testthat::test_path("test-data", "introduction.pptx"),
      url  = "https://github.com/knowusuboaky/RAGFlowChainR/raw/main/tests/testthat/test-data/introduction.pptx"
    ),
    txt  = list(
      path = testthat::test_path("test-data", "overview.txt"),
      url  = "https://github.com/knowusuboaky/RAGFlowChainR/raw/main/tests/testthat/test-data/overview.txt"
    )
  )

  # Download missing files
  lapply(test_files, function(file) download_test_file_if_needed(file$path, file$url))

  # --- PDF: sprint.pdf ---
  if (file.exists(test_files$pdf$path)) {
    pdf_res <- fetch_data(local_paths = test_files$pdf$path)
    expect_true(is.data.frame(pdf_res))
    expect_true(nrow(pdf_res) >= 1)
    expect_true(any(pdf_res$source_type == "pdf"))
  } else {
    skip("sprint.pdf not found in test-data folder.")
  }

  # --- DOCX: scrum.docx ---
  if (file.exists(test_files$docx$path)) {
    docx_res <- fetch_data(local_paths = test_files$docx$path)
    expect_true(is.data.frame(docx_res))
    expect_true(nrow(docx_res) >= 1)
    expect_true(any(docx_res$source_type == "docx"))
  } else {
    skip("scrum.docx not found in test-data folder.")
  }

  # --- PPTX: introduction.pptx ---
  if (file.exists(test_files$pptx$path)) {
    pptx_res <- fetch_data(local_paths = test_files$pptx$path)
    expect_true(is.data.frame(pptx_res))
    expect_true(nrow(pptx_res) >= 1)
    expect_true(any(pptx_res$source_type == "pptx"))
  } else {
    skip("introduction.pptx not found in test-data folder.")
  }

  # --- TXT: overview.txt ---
  if (file.exists(test_files$txt$path)) {
    txt_res <- fetch_data(local_paths = test_files$txt$path)
    expect_true(is.data.frame(txt_res))
    expect_true(nrow(txt_res) >= 1)
    expect_true(any(txt_res$source_type == "txt"))
  } else {
    skip("overview.txt not found in test-data folder.")
  }
})
