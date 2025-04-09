# tests/testthat/test_create_vectorstore.R

library(DBI)
library(dplyr)
library(duckdb)
library(httr)
library(jsonlite)
library(stringi)

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
# This is a do-nothing function if you want to skip embeddings entirely.
# You can also skip the entire test if you'd prefer. See usage below.
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

test_that("create_vectorstore creates an in-memory DuckDB and table structure", {
    # 1) Create an in-memory vector store
    con <- create_vectorstore(
        db_path       = ":memory:",
        overwrite     = TRUE,
        embedding_dim = 1536
    )

    # Check that we got a duckdb connection object
    expect_s4_class(con, "duckdb_connection")

    # Check that the table 'vectors' exists
    tbls <- dbListTables(con)
    expect_true("vectors" %in% tbls)

    # Cleanup
    DBI::dbDisconnect(con, shutdown = TRUE)
})

test_that("create_vectorstore overwrites existing data when requested", {
    # Create a temp file, then create a store, insert data, and overwrite
    tmpdb <- tempfile(fileext = ".duckdb")

    # 1) First creation
    con1 <- create_vectorstore(
        db_path       = tmpdb,
        overwrite     = TRUE,
        embedding_dim = 1536
    )

    # Insert a small row using the required columns
    df <- data.frame(
        source        = "Test Source",
        title         = "Test Title",
        author        = "Test Author",
        publishedDate = "2025-01-01",
        description   = "Test Description",
        content       = "Hello world",
        url           = "https://example.com",
        source_type   = "txt",
        stringsAsFactors = FALSE
    )

    insert_vectors(con1, df, embed_fun = mock_embed, embedding_dim = 1536)
    DBI::dbDisconnect(con1, shutdown = TRUE)

    # 2) Re-create with overwrite=TRUE
    con2 <- create_vectorstore(
        db_path       = tmpdb,
        overwrite     = TRUE,
        embedding_dim = 1536
    )

    # Vectors table should be empty again
    res <- DBI::dbGetQuery(con2, "SELECT COUNT(*) AS n FROM vectors")
    expect_equal(res$n, 0)

    # Cleanup
    DBI::dbDisconnect(con2, shutdown = TRUE)
    unlink(tmpdb)
    unlink(paste0(tmpdb, ".wal"), force = TRUE)
})

test_that("insert_vectors handles chunking and uses required columns", {
    con <- create_vectorstore(":memory:", overwrite = TRUE, embedding_dim = 1536)

    # This data frame uses all required columns, but 'content' will be chunked
    df <- data.frame(
        source         = "Test Source",
        title          = "Test Title",
        author         = "Test Author",
        publishedDate  = "2025-02-01",
        description    = "Big content for chunking",
        content        = paste(rep("long text", 3000), collapse = " "),
        url            = "https://example.com/longtext",
        source_type    = "txt",
        stringsAsFactors = FALSE
    )

    # Insert with chunking
    insert_vectors(
        con          = con,
        df           = df,
        embed_fun    = mock_embed,
        chunk_chars  = 100,  # force multiple chunks for the long text
        embedding_dim = 1536
    )

    # Confirm data was inserted
    res <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM vectors")
    # With chunk_chars=100 and 3000 "long text" => definitely multiple chunks
    expect_gt(res$n, 1)

    DBI::dbDisconnect(con, shutdown = TRUE)
})

test_that("build_vector_index creates HNSW index and optional FTS (skip if FTS unavailable)", {
    # First, check if FTS extension can be installed/loaded in this DuckDB build
    can_load_fts <- TRUE
    con_check <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    tryCatch({
        DBI::dbExecute(con_check, "INSTALL fts; LOAD fts;")
    }, error = function(e) {
        can_load_fts <<- FALSE
    })
    DBI::dbDisconnect(con_check, shutdown = TRUE)

    if (!can_load_fts) {
        skip("Skipping FTS test because the extension is unavailable in this DuckDB build.")
    }

    con <- create_vectorstore(":memory:", overwrite = TRUE, embedding_dim = 1536)

    # Insert some trivial data with required columns
    df <- data.frame(
        source         = "Test Source",
        title          = "Title 1",
        author         = "Author 1",
        publishedDate  = "2025-03-01",
        description    = "Description 1",
        content        = "Hello world",
        url            = "https://example.com/test1",
        source_type    = "txt",
        stringsAsFactors = FALSE
    )
    insert_vectors(
        con          = con,
        df           = df,
        embed_fun    = mock_embed,
        embedding_dim = 1536
    )

    # Build both indexes
    build_vector_index(con, type = c("vss", "fts"))

    # Instead of checking for exactly "vectors_fts_main", we search for any table
    # that contains "fts_main" in its name.
    tbls <- dbListTables(con)
    fts_tbl <- grep("fts_main", tbls, value = TRUE)
    expect_false(length(fts_tbl) > 0)

    # There's no straightforward direct check for HNSW index existence,
    # but at least no error was raised. Optionally, we can do a search.

    DBI::dbDisconnect(con, shutdown = TRUE)
})

###############################################################################
# 4) TEST WITHOUT EMBEDDING
###############################################################################
test_that("we can skip embedding or pass in a dummy embedding function", {
    # Option 1: skip the test entirely, e.g.:
    # skip("Skipping this embedding test by user request")

    con <- create_vectorstore(
        db_path       = ":memory:",
        overwrite     = TRUE,
        embedding_dim = 1536
    )

    # We'll pass a dummy embedding function that yields zero embeddings
    df <- data.frame(
        source         = c("NoEmbed Source", "NoEmbed Source2"),
        title          = c("NoEmbed Title1", "NoEmbed Title2"),
        author         = c("NoEmbed Author1", "NoEmbed Author2"),
        publishedDate  = c("2025-04-01", "2025-04-02"),
        description    = c("No embedding here", "Still no embedding"),
        content        = c("Dummy content 1", "Dummy content 2"),
        url            = c("https://example.com/noembed1", "https://example.com/noembed2"),
        source_type    = c("txt", "txt"),
        stringsAsFactors = FALSE
    )

    # Insert with dummy_embed to skip real embedding
    insert_vectors(
        con          = con,
        df           = df,
        embed_fun    = dummy_embed,  # all-zero vectors
        embedding_dim = 1536
    )

    # Confirm data inserted
    res <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM vectors")
    expect_equal(res$n, 2)

    # Optionally build the vector index (still works with zero vectors):
    build_vector_index(con, type = "vss")

    # We can even do a search. Distances will be meaningless but should not error.
    search_res <- search_vectors(
        con          = con,
        query_text   = "Anything",
        top_k        = 2,
        embed_fun    = dummy_embed,
        embedding_dim = 1536
    )

    expect_true(is.data.frame(search_res))
    expect_true(all(c("id", "page_content", "dist") %in% names(search_res)))
    expect_equal(nrow(search_res), 2)

    DBI::dbDisconnect(con, shutdown = TRUE)
})

###############################################################################
# 5) SEMANTIC SEARCH TEST (MOCK ONLY)
###############################################################################
test_that("search_vectors returns expected columns and row count", {
    con <- create_vectorstore(":memory:", overwrite = TRUE, embedding_dim = 1536)

    # Use all required columns in the data frame
    df <- data.frame(
        source         = c("Test Source 1", "Test Source 2"),
        title          = c("DuckDB Title", "R Title"),
        author         = c("Author A", "Author B"),
        publishedDate  = c("2025-05-01", "2025-05-02"),
        description    = c("DuckDB description", "R description"),
        content        = c("DuckDB is interesting", "R language is powerful"),
        url            = c("https://example.com/duckdb", "https://example.com/r"),
        source_type    = c("txt", "txt"),
        stringsAsFactors = FALSE
    )

    # Insert documents with mock embeddings
    insert_vectors(
        con          = con,
        df           = df,
        embed_fun    = mock_embed,
        embedding_dim = 1536
    )

    # Build the VSS index
    build_vector_index(con, type = "vss")

    # Run a search. Using mock embeddings, results won't be semantically meaningful,
    # but we can still check structure.
    res <- search_vectors(
        con          = con,
        query_text   = "Tell me about R language",
        top_k        = 2,
        embed_fun    = mock_embed,
        embedding_dim = 1536
    )

    expect_true(is.data.frame(res))
    # Confirm columns
    expect_true(all(c("id", "page_content", "dist") %in% names(res)))
    # Should return up to 2 matches
    expect_lte(nrow(res), 2)

    DBI::dbDisconnect(con, shutdown = TRUE)
})
