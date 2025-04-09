# tests/testthat/test_data_fetcher.R
library(testthat)

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
test_that("fetch_data can read multiple local files from test-data folder", {

    pdf_path  <- testthat::test_path("test-data", "sprint.pdf")
    docx_path <- testthat::test_path("test-data", "scrum.docx")
    pptx_path <- testthat::test_path("test-data", "introduction.pptx")
    txt_path  <- testthat::test_path("test-data", "overview.txt")

    # --- PDF: sprint.pdf ---
    if (file.exists(pdf_path)) {
        pdf_res <- fetch_data(local_paths = pdf_path)
        expect_true(is.data.frame(pdf_res))
        expect_true(nrow(pdf_res) >= 1)
        expect_true(any(pdf_res$source_type == "pdf"))
    } else {
        skip("sprint.pdf not found in test-data folder.")
    }

    # --- DOCX: scrum.docx ---
    if (file.exists(docx_path)) {
        docx_res <- fetch_data(local_paths = docx_path)
        expect_true(is.data.frame(docx_res))
        expect_true(nrow(docx_res) >= 1)
        expect_true(any(docx_res$source_type == "docx"))
    } else {
        skip("scrum.docx not found in test-data folder.")
    }

    # --- PPTX: introduction.pptx ---
    if (file.exists(pptx_path)) {
        pptx_res <- fetch_data(local_paths = pptx_path)
        expect_true(is.data.frame(pptx_res))
        expect_true(nrow(pptx_res) >= 1)
        expect_true(any(pptx_res$source_type == "pptx"))
    } else {
        skip("introduction.pptx not found in test-data folder.")
    }

    # --- TXT: overview.txt ---
    if (file.exists(txt_path)) {
        txt_res <- fetch_data(local_paths = txt_path)
        expect_true(is.data.frame(txt_res))
        expect_true(nrow(txt_res) >= 1)
        expect_true(any(txt_res$source_type == "txt"))
    } else {
        skip("overview.txt not found in test-data folder.")
    }
})
