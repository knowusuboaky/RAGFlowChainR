# tests/testthat/test_create_rag_chain.R

library(testthat)
library(DBI)
library(duckdb)

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
# 3) TESTS
###############################################################################

test_that("create_rag_chain works with local DuckDB and mock embedding", {
    # 1) Locate the pre-existing DuckDB file in the 'test-data' folder.
    #    Make sure you have placed 'my_vectorstore.duckdb' at:
    #    tests/testthat/test-data/my_vectorstore.duckdb
    vectorstore_path <- testthat::test_path("test-data", "my_vectorstore.duckdb")

    # If the file doesn't exist, skip.
    skip_if_not(file.exists(vectorstore_path), "my_vectorstore.duckdb not found in test-data folder.")

    # 2) Define a minimal mock LLM that returns a fixed answer.
    mock_llm <- function(prompt) {
        "Local DB LLM answer"
    }

    # 3) Create the RAG chain using the local DuckDB, specifying the mock_embed
    chain <- create_rag_chain(
        llm                      = mock_llm,
        vector_database_directory= vectorstore_path,
        method                   = "DuckDB",      # Only DuckDB is supported
        embedding_function       = mock_embed,    # Provide the mock embedding function
        embedding_dim            = 1536,          # Must match your stored vectors
        use_web_search           = FALSE          # Disable web calls
    )

    # 4) (Optional) Skip the chain invocation if environment var "RUN_CHAIN_INVOKE" != "true".
    skip_if_not(
        identical(Sys.getenv("RUN_CHAIN_INVOKE"), "true"),
        "Skipping chain invocation step (set RUN_CHAIN_INVOKE=true to run this part)."
    )

    # 5) Invoke the chain with a sample user query
    result <- chain$invoke("Tell me about local database testing")

    # 6) Validate results
    expect_true(is.list(result))
    expect_true("answer" %in% names(result))
    expect_equal(result$answer, "Local DB LLM answer")

    # The chat history should have two messages: human + assistant
    history <- chain$get_session_history()
    expect_equal(length(history), 2)
    expect_equal(history[[1]]$role, "human")
    expect_equal(history[[2]]$role, "assistant")

    # 7) Disconnect
    chain$disconnect()
})

test_that("create_rag_chain can ignore old chat history (local DuckDB)", {
    # 1) Locate the DuckDB file
    vectorstore_path <- testthat::test_path("test-data", "my_vectorstore.duckdb")
    skip_if_not(file.exists(vectorstore_path), "my_vectorstore.duckdb not found in test-data folder.")

    # 2) Define a mock LLM that returns an 'ignore' response
    mock_llm_ignore <- function(prompt) {
        "Answer ignoring old chat"
    }

    # 3) Create chain
    chain <- create_rag_chain(
        llm                      = mock_llm_ignore,
        vector_database_directory= vectorstore_path,
        method                   = "DuckDB",
        embedding_function       = mock_embed,  # use the mock embed
        embedding_dim            = 1536,
        use_web_search           = FALSE
    )

    # Build up some chat history
    chain$invoke("First question")
    chain$invoke("Second question")

    # (Optional) skip invocation if not desired
    skip_if_not(
        identical(Sys.getenv("RUN_CHAIN_INVOKE"), "true"),
        "Skipping chain invocation step (set RUN_CHAIN_INVOKE=true to run this part)."
    )

    # 4) Invoke with ignore_history = TRUE
    result <- chain$invoke("Third question, ignoring old chat", ignore_history = TRUE)
    expect_true(is.list(result))
    expect_true("answer" %in% names(result))
    expect_equal(result$answer, "Answer ignoring old chat")

    # 5) Final chat history still accumulates messages
    history <- chain$get_session_history()
    expect_true(length(history) >= 4)

    # Cleanup
    chain$disconnect()
})
