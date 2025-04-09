#' @title create_vectorstore.R Overview
#' @description
#' Provides tools to:
#' - Embed page content using OpenAI API (with support for chunking)
#' - Create a DuckDB-based vector store with support for HNSW indexing and optional FTS
#' - Insert documents with embedding vectors
#' - Perform approximate nearest-neighbor search using DuckDB's vss extension
#'
#' ## Required Packages
#' The following packages are required. Install them with:
#' \code{install.packages(c("dplyr", "DBI", "duckdb", "httr", "jsonlite", "stringi"))}
#'
#' @note Only `create_vectorstore()` is exported for use.
#' @name create_vectorstore
NULL

# Required libraries
library(DBI)
library(dplyr)
library(duckdb)
library(httr)
library(jsonlite)
library(stringi)

# ==============================================================================
# 1) EMBEDDING FUNCTIONS
# ==============================================================================

embed_openai <- function(
        x,
        model         = "text-embedding-ada-002",
        base_url      = "https://api.openai.com/v1",
        api_key       = Sys.getenv("OPENAI_API_KEY"),
        batch_size    = 20L,
        embedding_dim = 1536
) {
    if (missing(x) || is.null(x)) {
        force(model); force(base_url); force(api_key); force(batch_size); force(embedding_dim)
        return(function(texts) embed_openai(
            texts, model, base_url, api_key, batch_size, embedding_dim
        ))
    }
    if (is.data.frame(x)) {
        x[["embedding"]] <- embed_openai(
            x[["page_content"]], model, base_url, api_key, batch_size, embedding_dim
        )
        return(x)
    }
    if (!nzchar(api_key)) {
        stop("Please set OPENAI_API_KEY in your environment.")
    }
    if (!length(x)) {
        return(matrix(numeric(0), nrow = 0, ncol = embedding_dim))
    }

    body <- list(model = model, input = as.list(x))
    res <- httr::POST(
        url = file.path(base_url, "embeddings"),
        encode = "json",
        body   = body,
        httr::add_headers(Authorization = paste("Bearer", api_key))
    )
    if (httr::http_error(res)) {
        stop("OpenAI API error:\n", httr::content(res, "text"))
    }
    parsed <- httr::content(res, "parsed")
    emb_list <- lapply(parsed$data, function(z) z$embedding)
    emb_mat <- do.call(rbind, emb_list)
    storage.mode(emb_mat) <- "double"
    if (ncol(emb_mat) != embedding_dim) {
        stop(sprintf(
            "OpenAI returned %d-d embeddings, but embedding_dim=%d. Adjust accordingly.",
            ncol(emb_mat), embedding_dim
        ))
    }
    emb_mat
}

# ==============================================================================
# 2) CREATE / CONNECT DUCKDB
# ==============================================================================

#' Create a DuckDB-Based Vector Store
#'
#' Initializes or connects to a DuckDB instance and prepares a table to store embeddings.
#' Optionally wipes existing data and installs required extensions for vector search.
#'
#' @param db_path Path to the DuckDB file. Use ":memory:" for an in-memory DB.
#' @param overwrite Logical, whether to drop and recreate the table if it already exists.
#' @param embedding_dim Integer, the dimensionality of the embedding vectors.
#'
#' @return A DuckDB connection object.
#' @export
create_vectorstore <- function(
        db_path       = ":memory:",
        overwrite     = FALSE,
        embedding_dim = 1536
) {
    if (db_path != ":memory:" && file.exists(db_path) && overwrite) {
        unlink(db_path)
        wal <- paste0(db_path, ".wal")
        if (file.exists(wal)) unlink(wal)
    }

    con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = FALSE)
    DBI::dbExecute(con, "INSTALL vss; LOAD vss;")

    if (overwrite) {
        DBI::dbExecute(con, "DROP TABLE IF EXISTS vectors;")
        DBI::dbExecute(con, "DROP SEQUENCE IF EXISTS vector_seq;")
    }

    DBI::dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS vector_seq START 1;")
    create_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS vectors(
      id           INT   DEFAULT nextval('vector_seq'),
      page_content VARCHAR,
      embedding    FLOAT[%d]
    );
  ", embedding_dim)
    DBI::dbExecute(con, create_sql)

    con
}

connect_vectorstore <- function(db_path = ":memory:", read_only = FALSE) {
    con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = read_only)
    DBI::dbExecute(con, "INSTALL vss; LOAD vss;")
    con
}

# ==============================================================================
# 3) INSERT WITH CHUNKING
# ==============================================================================

chunk_content_approx <- function(content, chunk_chars = 12000) {
    n <- nchar(content)
    if (n <= chunk_chars) {
        return(content)
    }
    starts <- seq(1, n, by = chunk_chars)
    ends   <- pmin(starts + chunk_chars - 1, n)
    mapply(substr, content, starts, ends, USE.NAMES = FALSE)
}

insert_vectors <- function(
        con,
        df,
        embed_fun     = embed_openai(),
        chunk_chars   = 12000,
        embedding_dim = 1536
) {
    if (!"page_content" %in% names(df)) {
        if ("content" %in% names(df)) {
            df$page_content <- df$content
        } else {
            stop("Data frame must have 'page_content' or 'content' column.")
        }
    }

    expanded <- list()
    idx <- 1
    for (i in seq_len(nrow(df))) {
        row_i <- df[i, ]
        splitted <- chunk_content_approx(row_i$page_content, chunk_chars)
        for (chunked_content in splitted) {
            row_cp <- row_i
            row_cp$page_content <- chunked_content
            expanded[[idx]] <- row_cp
            idx <- idx + 1
        }
    }
    df_expanded <- dplyr::bind_rows(expanded)
    if (!nrow(df_expanded)) return(invisible(NULL))

    if (!"embedding" %in% names(df_expanded)) {
        emb_mat <- embed_fun(df_expanded$page_content)
        if (ncol(emb_mat) != embedding_dim) {
            stop(sprintf(
                "Embeddings dimension is %d, but embedding_dim=%d. Adjust code or embed_fun config.",
                ncol(emb_mat), embedding_dim
            ))
        }
        storage.mode(emb_mat) <- "double"
        df_expanded$embedding <- emb_mat
    } else {
        if (is.matrix(df_expanded$embedding)) {
            if (ncol(df_expanded$embedding) != embedding_dim) {
                stop(sprintf(
                    "Embeddings dimension is %d, but embedding_dim=%d. Mismatch.",
                    ncol(df_expanded$embedding), embedding_dim
                ))
            }
            storage.mode(df_expanded$embedding) <- "double"
        } else {
            df_expanded$embedding <- lapply(df_expanded$embedding, function(vec) {
                if (length(vec) != embedding_dim) {
                    stop(sprintf(
                        "One row has embedding dimension %d, but embedding_dim=%d. Mismatch.",
                        length(vec), embedding_dim
                    ))
                }
                storage.mode(vec) <- "double"
                vec
            })
        }
    }

    n <- nrow(df_expanded)
    rows_sql <- character(n)
    for (i in seq_len(n)) {
        content_esc <- DBI::dbQuoteString(con, df_expanded$page_content[i])
        if (is.matrix(df_expanded$embedding)) {
            e_vec <- df_expanded$embedding[i, ]
        } else {
            e_vec <- df_expanded$embedding[[i]]
        }
        e_str <- paste(e_vec, collapse = ",")
        e_expr <- sprintf("CAST(array_value(%s) AS FLOAT[])", e_str)
        rows_sql[i] <- sprintf("(%s,%s)", content_esc, e_expr)
    }

    insert_sql <- sprintf("
    INSERT INTO vectors(page_content, embedding)
    VALUES %s
  ", paste(rows_sql, collapse = ",\n"))

    DBI::dbExecute(con, insert_sql)
    invisible(NULL)
}

# ==============================================================================
# 4) BUILD INDEX & SEARCH
# ==============================================================================

build_vector_index <- function(store, type = c("vss", "fts")) {
    con <- if (inherits(store, "DBIConnection")) store else store
    type <- match.arg(type, several.ok = TRUE)

    if ("vss" %in% type) {
        DBI::dbExecute(con, "SET hnsw_enable_experimental_persistence = true;")
        DBI::dbExecute(con, "DROP INDEX IF EXISTS idx_vectors_embedding;")
        DBI::dbExecute(con, "
      CREATE INDEX idx_vectors_embedding
      ON vectors
      USING HNSW(embedding);
    ")
    }

    if ("fts" %in% type) {
        DBI::dbExecute(con, "INSTALL fts; LOAD fts;")
        DBI::dbExecute(con, "
      PRAGMA create_fts_index(
        'vectors',
        'id',
        'page_content',
        overwrite=1
      );
    ")
    }

    invisible(store)
}

search_vectors <- function(
        con,
        query_text,
        top_k         = 5,
        embed_fun     = embed_openai(),
        embedding_dim = 1536
) {
    q_emb <- embed_fun(query_text)
    if (ncol(q_emb) != embedding_dim) {
        stop(sprintf(
            "Query embedding dimension is %d, but embedding_dim=%d. Mismatch.",
            ncol(q_emb), embedding_dim
        ))
    }
    storage.mode(q_emb) <- "double"

    DBI::dbExecute(con, "DROP TABLE IF EXISTS __temp_query__;")
    create_tmp_sql <- sprintf("
    CREATE TEMP TABLE __temp_query__ (
      embedding FLOAT[%d]
    );
  ", embedding_dim)
    DBI::dbExecute(con, create_tmp_sql)

    emb_str <- paste(q_emb[1, ], collapse = ",")
    arr_expr <- sprintf("array_value(%s)", emb_str)
    insert_tmp_sql <- sprintf("
    INSERT INTO __temp_query__(embedding)
    VALUES (%s)
  ", arr_expr)
    DBI::dbExecute(con, insert_tmp_sql)

    sql <- sprintf("
    SELECT v.id, v.page_content, v.embedding <=> (SELECT embedding FROM __temp_query__) AS dist
    FROM vectors v
    ORDER BY dist ASC
    LIMIT %d;
  ", top_k)

    res <- DBI::dbGetQuery(con, sql)
    DBI::dbExecute(con, "DROP TABLE IF EXISTS __temp_query__;")

    res
}
