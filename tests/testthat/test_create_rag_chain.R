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

test_that("create_rag_chain supports VectrixDB backend via adapter", {
  mock_llm <- function(prompt) "vectrix answer"
  fake_store <- list(
    close = function() invisible(NULL)
  )

  local_mocked_bindings(
    vectrixdb_is_available = function() TRUE,
    connect_vectrix_store = function(vector_database_directory) fake_store,
    search_vectrix = function(vdb, query_text, top_k = 5, mode = "hybrid") {
      out <- data.frame(
        id = "doc1",
        page_content = "Vectrix document",
        score = 0.99,
        stringsAsFactors = FALSE
      )
      out$metadata <- list(list(source = "mock"))
      out
    },
    .package = "RAGFlowChainR"
  )

  chain <- create_rag_chain(
    llm = mock_llm,
    vector_database_directory = "dummy_collection",
    method = "VectrixDB",
    use_web_search = FALSE
  )

  out <- chain$invoke("Tell me about R")
  expect_equal(out$answer, "vectrix answer")
  expect_true(length(out$documents) >= 1)
  expect_equal(out$documents[[1]]$page_content, "Vectrix document")
  expect_silent(chain$disconnect())
})

test_that("create_rag_chain supports Qdrant backend via adapter", {
  mock_llm <- function(prompt) "qdrant answer"

  local_mocked_bindings(
    connect_qdrant_store = function(vector_database_directory) list(url = "mock"),
    search_qdrant = function(store, query_text, top_k = 5, embed_fun = NULL, embedding_dim = 1536) {
      out <- data.frame(
        id = "doc_qdrant",
        page_content = "Qdrant document",
        score = 0.97,
        stringsAsFactors = FALSE
      )
      out$metadata <- list(list(source = "mock"))
      out
    },
    .package = "RAGFlowChainR"
  )

  chain <- create_rag_chain(
    llm = mock_llm,
    vector_database_directory = "https://mock-qdrant|collection",
    method = "Qdrant",
    use_web_search = FALSE
  )

  out <- chain$invoke("Tell me about R")
  expect_equal(out$answer, "qdrant answer")
  expect_equal(out$documents[[1]]$page_content, "Qdrant document")
})

test_that("create_rag_chain supports Pinecone backend via adapter", {
  mock_llm <- function(prompt) "pinecone answer"

  local_mocked_bindings(
    connect_pinecone_store = function(vector_database_directory) list(url = "mock"),
    search_pinecone = function(store, query_text, top_k = 5, embed_fun = NULL, embedding_dim = 1536) {
      out <- data.frame(
        id = "doc_pinecone",
        page_content = "Pinecone document",
        score = 0.95,
        stringsAsFactors = FALSE
      )
      out$metadata <- list(list(source = "mock"))
      out
    },
    .package = "RAGFlowChainR"
  )

  chain <- create_rag_chain(
    llm = mock_llm,
    vector_database_directory = "https://mock-pinecone|namespace",
    method = "Pinecone",
    use_web_search = FALSE
  )

  out <- chain$invoke("Tell me about R")
  expect_equal(out$answer, "pinecone answer")
  expect_equal(out$documents[[1]]$page_content, "Pinecone document")
})

test_that("create_rag_chain supports Weaviate backend via adapter", {
  mock_llm <- function(prompt) "weaviate answer"

  local_mocked_bindings(
    connect_weaviate_store = function(vector_database_directory) list(url = "mock"),
    search_weaviate = function(store, query_text, top_k = 5, embed_fun = NULL, embedding_dim = 1536) {
      out <- data.frame(
        id = "doc_weaviate",
        page_content = "Weaviate document",
        score = 0.94,
        stringsAsFactors = FALSE
      )
      out$metadata <- list(list(source = "mock"))
      out
    },
    .package = "RAGFlowChainR"
  )

  chain <- create_rag_chain(
    llm = mock_llm,
    vector_database_directory = "https://mock-weaviate|Document",
    method = "Weaviate",
    use_web_search = FALSE
  )

  out <- chain$invoke("Tell me about R")
  expect_equal(out$answer, "weaviate answer")
  expect_equal(out$documents[[1]]$page_content, "Weaviate document")
})

test_that("create_rag_chain supports Elasticsearch backend via adapter", {
  mock_llm <- function(prompt) "elastic answer"

  local_mocked_bindings(
    connect_elasticsearch_store = function(vector_database_directory) list(url = "mock"),
    search_elasticsearch = function(store, query_text, top_k = 5, embed_fun = NULL, embedding_dim = 1536) {
      out <- data.frame(
        id = "doc_elastic",
        page_content = "Elasticsearch document",
        score = 0.93,
        stringsAsFactors = FALSE
      )
      out$metadata <- list(list(source = "mock"))
      out
    },
    .package = "RAGFlowChainR"
  )

  chain <- create_rag_chain(
    llm = mock_llm,
    vector_database_directory = "https://mock-elastic|vectors|embedding",
    method = "Elasticsearch",
    use_web_search = FALSE
  )

  out <- chain$invoke("Tell me about R")
  expect_equal(out$answer, "elastic answer")
  expect_equal(out$documents[[1]]$page_content, "Elasticsearch document")
})

test_that("create_rag_chain errors clearly when VectrixDB is missing", {
  local_mocked_bindings(
    vectrixdb_is_available = function() FALSE,
    .package = "RAGFlowChainR"
  )

  expect_error(
    create_rag_chain(
      llm = function(prompt) "ok",
      vector_database_directory = "anything",
      method = "VectrixDB",
      use_web_search = FALSE
    ),
    "requires package 'VectrixDB'"
  )
})
