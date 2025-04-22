# tests/testthat/test_create_rag_chain.R

library(testthat)
library(DBI)
library(duckdb)
library(httr)
library(jsonlite)
library(stringi)
library(dplyr)


###############################################################################
# 1) MOCK EMBEDDING FUNCTION
###############################################################################
# A mock embedding function that returns a deterministic random embedding
# of size embedding_dim for each text. This avoids external API calls,
# making it CRAN-safe. If you want to test with OpenAI, you can conditionally
# skip or override this function.
mock_embed <- function(x, embedding_dim = 1536) {
    set.seed(123)  # So tests are deterministic
    if (is.data.frame(x) && "page_content" %in% names(x)) {
        # Return the same data frame but add a random embedding column
        emb <- mock_embed(x$page_content, embedding_dim = embedding_dim)
        x[["embedding"]] <- emb
        return(x)
    }
    if (!length(x)) {
        return(matrix(numeric(0), nrow = 0, ncol = embedding_dim))
    }
    n_obs <- length(x)
    matrix(runif(n_obs * embedding_dim), nrow = n_obs, ncol = embedding_dim)
}

###############################################################################
# 2) OPTIONAL: DUMMY EMBEDDING FUNCTION THAT SKIPS
###############################################################################
dummy_embed <- function(x, embedding_dim = 1536) {
    # Return the correct shape but no actual embeddings
    if (is.data.frame(x) && "page_content" %in% names(x)) {
        x[["embedding"]] <- matrix(0, nrow = nrow(x), ncol = embedding_dim)
        return(x)
    }
    if (!length(x)) {
        return(matrix(numeric(0), nrow = 0, ncol = embedding_dim))
    }
    n_obs <- length(x)
    matrix(0, nrow = n_obs, ncol = embedding_dim)
}


###############################################################################
# 3) HELPER: Download vectorstore if missing (skip on CRAN)
###############################################################################
download_vectorstore_if_needed <- function(local_path, github_url) {
  if (!file.exists(local_path)) {
    message("Downloading vectorstore from GitHub...")
    dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
    download.file(github_url, destfile = local_path, mode = "wb", quiet = TRUE)
  }
}

###############################################################################
# 4) TESTS
###############################################################################

test_that("create_rag_chain works with local DuckDB and mock embedding", {
  local_path <- testthat::test_path("test-data", "my_vectorstore.duckdb")
  github_url <- "https://github.com/knowusuboaky/RAGFlowChainR/raw/main/tests/testthat/test-data/my_vectorstore.duckdb"

  # Only download if not running on CRAN
  skip_on_cran()
  download_vectorstore_if_needed(local_path, github_url)
  skip_if_not(file.exists(local_path), "my_vectorstore.duckdb not found or failed to download.")

  mock_llm <- function(prompt) {
    "Local DB LLM answer"
  }

  chain <- create_rag_chain(
    llm                      = mock_llm,
    vector_database_directory= local_path,
    method                   = "DuckDB",
    embedding_function       = mock_embed,
    embedding_dim            = 1536,
    use_web_search           = FALSE
  )

  skip_if_not(
    identical(Sys.getenv("RUN_CHAIN_INVOKE"), "true"),
    "Skipping chain invocation step (set RUN_CHAIN_INVOKE=true to run this part)."
  )

  result <- chain$invoke("Tell me about local database testing")
  expect_true(is.list(result))
  expect_true("answer" %in% names(result))
  expect_equal(result$answer, "Local DB LLM answer")

  history <- chain$get_session_history()
  expect_equal(length(history), 2)
  expect_equal(history[[1]]$role, "human")
  expect_equal(history[[2]]$role, "assistant")

  chain$disconnect()
})


test_that("create_rag_chain can ignore old chat history (local DuckDB)", {
  local_path <- testthat::test_path("test-data", "my_vectorstore.duckdb")
  github_url <- "https://github.com/knowusuboaky/RAGFlowChainR/raw/main/tests/testthat/test-data/my_vectorstore.duckdb"

  skip_on_cran()
  download_vectorstore_if_needed(local_path, github_url)
  skip_if_not(file.exists(local_path), "my_vectorstore.duckdb not found or failed to download.")

  mock_llm_ignore <- function(prompt) {
    "Answer ignoring old chat"
  }

  chain <- create_rag_chain(
    llm                      = mock_llm_ignore,
    vector_database_directory= local_path,
    method                   = "DuckDB",
    embedding_function       = mock_embed,
    embedding_dim            = 1536,
    use_web_search           = FALSE
  )

  chain$invoke("First question")
  chain$invoke("Second question")

  skip_if_not(
    identical(Sys.getenv("RUN_CHAIN_INVOKE"), "true"),
    "Skipping chain invocation step (set RUN_CHAIN_INVOKE=true to run this part)."
  )

  result <- chain$invoke("Third question, ignoring old chat", ignore_history = TRUE)
  expect_true(is.list(result))
  expect_true("answer" %in% names(result))
  expect_equal(result$answer, "Answer ignoring old chat")

  history <- chain$get_session_history()
  expect_true(length(history) >= 4)

  chain$disconnect()
})
