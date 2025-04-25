# tests/testthat/test_create_rag_chain.R

library(testthat)
library(DBI)
library(duckdb)
library(httr)
library(jsonlite)
library(stringi)
library(dplyr)

# ─── mock embedder ──────────────────────────────────────────────────────────
mock_embed <- function(x, embedding_dim = 1536) {
  set.seed(123)
  if (is.data.frame(x) && "page_content" %in% names(x)) {
    x$embedding <- mock_embed(x$page_content, embedding_dim); return(x)
  }
  if (!length(x)) return(matrix(numeric(0), 0, embedding_dim))
  matrix(runif(length(x) * embedding_dim), nrow = length(x))
}

# ─── helper to fetch test DB (skipped on CRAN) ───────────────────────────────
download_vectorstore_if_needed <- function(path, url) {
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    try(download.file(url, path, mode = "wb", quiet = TRUE), silent = TRUE)
  }
}

# ───────────────────────────────────────── tests ──────────────────────────────
test_that("create_rag_chain answers with mock LLM silently", {
  skip_on_cran()
  local_db <- test_path("test-data", "my_vectorstore.duckdb")
  github   <- "https://github.com/knowusuboaky/RAGFlowChainR/raw/main/tests/testthat/test-data/my_vectorstore.duckdb"
  download_vectorstore_if_needed(local_db, github)
  skip_if_not(file.exists(local_db), "vectorstore missing")

  mock_llm <- function(prompt) "LLM answer"
  chain <- suppressWarnings(
    create_rag_chain(
      llm                        = mock_llm,
      vector_database_directory  = local_db,
      embedding_function         = mock_embed,
      use_web_search             = FALSE
    )
  )
  expect_silent(
    suppressWarnings(out <- chain$invoke("any question"))
  )
  expect_equal(out$answer, "LLM answer")
  chain$disconnect()
})

test_that("ignore_history flag respected silently", {
  skip_on_cran()
  local_db <- test_path("test-data", "my_vectorstore.duckdb")
  github   <- "https://github.com/knowusuboaky/RAGFlowChainR/raw/main/tests/testthat/test-data/my_vectorstore.duckdb"
  download_vectorstore_if_needed(local_db, github)
  skip_if_not(file.exists(local_db), "vectorstore missing")

  mock_llm <- function(prompt) "fresh"
  chain <- suppressWarnings(
    create_rag_chain(
      llm                        = mock_llm,
      vector_database_directory  = local_db,
      embedding_function         = mock_embed,
      use_web_search             = FALSE
    )
  )
  chain$invoke("first")
  expect_silent(
    suppressWarnings(res <- chain$invoke("second", ignore_history = TRUE))
  )
  expect_equal(res$answer, "fresh")
  chain$disconnect()
})
