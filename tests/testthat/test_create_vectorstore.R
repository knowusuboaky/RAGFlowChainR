# tests/testthat/test_create_vectorstore.R

library(testthat)
library(DBI)
library(dplyr)
library(duckdb)
library(httr)
library(jsonlite)
library(stringi)

# ───────────────────────────────── mock / dummy embedders ───────────────────
mock_embed <- function(x, embedding_dim = 1536) {
  set.seed(123)
  if (is.data.frame(x) && "page_content" %in% names(x)) {
    x[["embedding"]] <- mock_embed(x$page_content, embedding_dim)
    return(x)
  }
  if (!length(x)) return(matrix(numeric(0), 0, embedding_dim))
  matrix(runif(length(x) * embedding_dim), nrow = length(x))
}

dummy_embed <- function(x, embedding_dim = 1536) {
  if (is.data.frame(x) && "page_content" %in% names(x)) {
    x[["embedding"]] <- matrix(0, nrow(x), embedding_dim)
    return(x)
  }
  if (!length(x)) return(matrix(numeric(0), 0, embedding_dim))
  matrix(0, nrow = length(x), ncol = embedding_dim)
}

# ───────────────────────────────────────── tests ────────────────────────────
test_that("create_vectorstore builds in-memory DB silently", {
  con <- suppressWarnings(create_vectorstore(":memory:", TRUE, 1536))
  expect_s4_class(con, "duckdb_connection")
  expect_true("vectors" %in% dbListTables(con))
  dbDisconnect(con, shutdown = TRUE)
})

test_that("overwrite empties previous data silently", {
  tmp <- tempfile(fileext = ".duckdb")
  con1 <- suppressWarnings(create_vectorstore(tmp, TRUE, 1536))
  insert_vectors(con1, data.frame(page_content = "x"), mock_embed)
  dbDisconnect(con1, shutdown = TRUE)

  con2 <- suppressWarnings(create_vectorstore(tmp, TRUE, 1536))
  expect_equal(
    dbGetQuery(con2, "SELECT COUNT(*) AS n FROM vectors")$n,
    0
  )
  dbDisconnect(con2, shutdown = TRUE)
  unlink(c(tmp, paste0(tmp, ".wal")), force = TRUE)
})

test_that("insert_vectors chunks long text silently", {
  con <- suppressWarnings(create_vectorstore(":memory:", TRUE, 1536))
  long <- paste(rep("chunk", 2000), collapse = " ")
  expect_silent(
    insert_vectors(
      con,
      data.frame(page_content = long),
      mock_embed,
      chunk_chars = 100
    )
  )
  expect_gt(
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM vectors")$n,
    1
  )
  dbDisconnect(con, shutdown = TRUE)
})

test_that("build_vector_index succeeds silently (vss/fts)", {
  # Check whether FTS can load
  has_fts <- TRUE
  tmp <- dbConnect(duckdb::duckdb(), ":memory:")
  tryCatch(
    dbExecute(tmp, "INSTALL fts; LOAD fts;"),
    error = function(e) {
      message("FTS not available: ", conditionMessage(e))
      has_fts <<- FALSE
    }
  )
  dbDisconnect(tmp, shutdown = TRUE)

  if (!has_fts) {
    skip("Skipping FTS test: DuckDB FTS extension not available on this platform.")
  }

  con <- suppressWarnings(create_vectorstore(":memory:", TRUE, 1536))
  insert_vectors(con, data.frame(page_content = "y"), mock_embed)

  expect_silent(
    suppressWarnings(build_vector_index(con, c("vss", "fts")))
  )

  dbDisconnect(con, shutdown = TRUE)
})


test_that("dummy embeddings round-trip silently", {
  con <- suppressWarnings(create_vectorstore(":memory:", TRUE, 1536))
  expect_silent(
    insert_vectors(con, data.frame(page_content = c("a", "b")), dummy_embed)
  )
  res <- suppressWarnings(search_vectors(con, "a", 2, dummy_embed, 1536))
  expect_true(all(c("id", "page_content", "dist") %in% names(res)))
  dbDisconnect(con, shutdown = TRUE)
})

test_that("search_vectors returns expected structure silently", {
  con <- suppressWarnings(create_vectorstore(":memory:", TRUE, 1536))
  df  <- data.frame(page_content = c("DuckDB", "R language"))
  insert_vectors(con, df, mock_embed)
  expect_silent(
    suppressWarnings(build_vector_index(con, "vss"))
  )
  res <- suppressWarnings(search_vectors(con, "DuckDB", 2, mock_embed, 1536))
  expect_lte(nrow(res), 2)
  dbDisconnect(con, shutdown = TRUE)
})
