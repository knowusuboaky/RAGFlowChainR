# =============================================================================
# 1) EMBEDDING
# =============================================================================
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
    return(function(txt) embed_openai(
      txt, model, base_url, api_key, batch_size, embedding_dim))
  }

  if (is.data.frame(x)) {
    x[["embedding"]] <- embed_openai(
      x[["page_content"]], model, base_url, api_key, batch_size, embedding_dim
    )
    return(x)
  }

  if (!nzchar(api_key))
    stop("Set OPENAI_API_KEY in your environment.", call. = FALSE)
  if (!length(x))
    return(matrix(numeric(0), 0, embedding_dim))

  res <- httr::POST(
    url    = file.path(base_url, "embeddings"),
    body   = list(model = model, input = as.list(x)),
    encode = "json",
    httr::add_headers(Authorization = paste("Bearer", api_key))
  )
  if (httr::http_error(res))
    stop("OpenAI API error:\n", httr::content(res, "text"), call. = FALSE)

  emb <- do.call(rbind, lapply(httr::content(res, "parsed")$data, `[[`, "embedding"))
  storage.mode(emb) <- "double"
  if (ncol(emb) != embedding_dim)
    stop(sprintf("OpenAI returned %d-d embeddings (expected %d).", ncol(emb), embedding_dim))
  emb
}

# =============================================================================
# 2) CREATE / CONNECT DUCKDB
# =============================================================================
#' Create a DuckDB-based vector store
#'
#' Initializes a DuckDB database connection for storing embedded documents,
#' with optional support for the experimental `vss` extension.
#'
#' @param db_path Path to the DuckDB file. Use `":memory:"` to create an in-memory database.
#' @param overwrite Logical; if `TRUE`, deletes any existing DuckDB file or table.
#' @param embedding_dim Integer; the dimensionality of the vector embeddings to store.
#' @param load_vss Logical; whether to load the experimental `vss` extension.
#'   This defaults to `TRUE`, but is forced to `FALSE` during CRAN checks.
#'
#' @return A live DuckDB connection object. Be sure to manually disconnect with:
#' \code{DBI::dbDisconnect(con, shutdown = TRUE)}
#'
#' @details
#' This function is part of the vector-store utilities for:
#' \itemize{
#'   \item Embedding text via the OpenAI API
#'   \item Storing and chunking documents in DuckDB
#'   \item Building `HNSW` and `FTS` indexes
#'   \item Running nearest-neighbour search over vector embeddings
#' }
#'
#' Only \code{create_vectorstore()} is exported; helpers like \code{insert_vectors()}, \code{build_vector_index()},
#' and \code{search_vectors()} are internal but designed to be composable.
#'
#' @examples
#' \dontrun{
#' # Create vector store
#' con <- create_vectorstore("tests/testthat/test-data/my_vectors.duckdb", overwrite = TRUE)
#'
#' # Assume response is output from fetch_data()
#' docs <- data.frame(head(response))
#'
#' # Insert documents with embeddings
#' insert_vectors(
#'   con = con,
#'   df = docs,
#'   embed_fun = embed_openai(),
#'   chunk_chars = 12000
#' )
#'
#' # Build vector + FTS indexes
#' build_vector_index(con, type = c("vss", "fts"))
#'
#' # Perform vector search
#' response <- search_vectors(con, query_text = "Tell me about R?", top_k = 5)
#' }
#'
#' @name create_vectorstore
#' @export
NULL

# ───────────────────────────── dependencies ────────────────────────────────
library(DBI)
library(dplyr)
library(duckdb)
library(httr)
library(jsonlite)
library(stringi)

create_vectorstore <- function(
    db_path       = ":memory:",
    overwrite     = FALSE,
    embedding_dim = 1536,
    load_vss      = identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "")
) {
  if (db_path != ":memory:" && overwrite && file.exists(db_path))
    unlink(c(db_path, paste0(db_path, ".wal")))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)

  ## ── extension handling ───────────────────────────────────────────────────
  have_vss <- FALSE
  if (load_vss) {
    try({
      exts <- DBI::dbGetQuery(con, "PRAGMA show_extensions()")$name
      if ("vss" %in% exts) {
        DBI::dbExecute(con, "LOAD vss;")
        have_vss <- TRUE
      } else if (interactive() &&
                 !identical(Sys.getenv("_R_CHECK_CRAN_INCOMING_"), "true")) {
        DBI::dbExecute(con, "INSTALL vss; LOAD vss;")
        have_vss <- TRUE
      }
    }, silent = TRUE)
    if (!have_vss)
      warning("Could not load DuckDB vss extension; falling back to FLOAT[] column.")
  }

  ## ── schema ───────────────────────────────────────────────────────────────
  if (overwrite) {
    DBI::dbExecute(con, "DROP TABLE IF EXISTS vectors;")
    DBI::dbExecute(con, "DROP SEQUENCE IF EXISTS vector_seq;")
  }
  DBI::dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS vector_seq START 1;")

  col_type <- if (have_vss) sprintf("VECTOR[%d]", embedding_dim) else "FLOAT[]"
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE TABLE IF NOT EXISTS vectors(
         id INT DEFAULT nextval('vector_seq'),
         page_content VARCHAR,
         embedding    %s
       );", col_type)
  )
  invisible(con)
}

connect_vectorstore <- function(db_path = ":memory:", read_only = FALSE) {
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}

# =============================================================================
# 3) INSERT WITH CHUNKING
# =============================================================================
chunk_content_approx <- function(txt, chunk_chars = 12000) {
  n <- nchar(txt, "bytes")
  if (n <= chunk_chars) return(txt)
  starts <- seq.int(1L, n, by = chunk_chars)
  ends   <- pmin(starts + chunk_chars - 1L, n)
  Map(substr, txt, starts, ends, USE.NAMES = FALSE)
}

insert_vectors <- function(
    con,
    df,
    embed_fun     = embed_openai(),
    chunk_chars   = 12000,
    embedding_dim = 1536
) {
  if (!"page_content" %in% names(df)) {
    if ("content" %in% names(df)) df$page_content <- df$content
    else stop("Data frame must contain 'page_content' or 'content'.")
  }

  ## ── chunking ─────────────────────────────────────────────────────────────
  expanded <- list(); idx <- 1L
  for (i in seq_len(nrow(df))) {
    row_i <- df[i, , drop = FALSE]
    for (txt in chunk_content_approx(row_i$page_content, chunk_chars)) {
      row              <- row_i
      row$page_content <- txt
      expanded[[idx]]  <- row
      idx <- idx + 1L
    }
  }
  df_exp <- dplyr::bind_rows(expanded)
  if (!nrow(df_exp)) return(invisible(NULL))

  ## ── embeddings ───────────────────────────────────────────────────────────
  if (!"embedding" %in% names(df_exp)) {
    emb <- embed_fun(df_exp$page_content)
    if (ncol(emb) != embedding_dim)
      stop("Embedding dimensionality mismatch.")
    storage.mode(emb) <- "double"
    df_exp$embedding  <- emb
  } else if (is.matrix(df_exp$embedding)) {
    if (ncol(df_exp$embedding) != embedding_dim)
      stop("Embedding dimensionality mismatch.")
    storage.mode(df_exp$embedding) <- "double"
  } else {
    df_exp$embedding <- lapply(df_exp$embedding, function(v) {
      if (length(v) != embedding_dim)
        stop("Embedding dimensionality mismatch.")
      storage.mode(v) <- "double"; v
    })
  }

  ## ── pick value constructor: array_value() vs list_value() ────────────────
  table_types <- DBI::dbGetQuery(con, "PRAGMA table_info('vectors')")$type
  val_fun     <- if (any(grepl("VECTOR", table_types, fixed = TRUE)))
    "array_value" else "list_value"

  rows_sql <- vapply(seq_len(nrow(df_exp)), function(i) {
    esc <- DBI::dbQuoteString(con, df_exp$page_content[i])
    vec <- if (is.matrix(df_exp$embedding)) df_exp$embedding[i, ]
    else df_exp$embedding[[i]]
    sprintf("(%s, %s(%s))", esc, val_fun, paste(vec, collapse = ","))
  }, character(1))

  DBI::dbExecute(
    con,
    sprintf("INSERT INTO vectors(page_content, embedding) VALUES %s",
            paste(rows_sql, collapse = ",\n"))
  )
  invisible(NULL)
}

# =============================================================================
# 4) INDEX & SEARCH
# =============================================================================
build_vector_index <- function(store, type = c("vss", "fts")) {
  con  <- if (inherits(store, "DBIConnection")) store else store
  type <- match.arg(type, several.ok = TRUE)

  # does the vectors table use the VECTOR type?
  tbl_types  <- DBI::dbGetQuery(con, "PRAGMA table_info('vectors')")$type
  have_vss   <- any(grepl("VECTOR", tbl_types, fixed = TRUE))

  ## ── HNSW via vss ─────────────────────────────────────────────────────────
  if ("vss" %in% type) {
    if (!have_vss) {
      warning("vss extension not available in this store; skipping HNSW index.")
    } else {
      DBI::dbExecute(con, "SET hnsw_enable_experimental_persistence = true;")
      DBI::dbExecute(con, "DROP INDEX IF EXISTS idx_vectors_embedding;")
      DBI::dbExecute(con, "
        CREATE INDEX idx_vectors_embedding
        ON vectors USING HNSW(embedding);")
    }
  }

  ## ── Full-text search index ──────────────────────────────────────────────
  if ("fts" %in% type) {
    DBI::dbExecute(con, "INSTALL fts; LOAD fts;")
    DBI::dbExecute(con, "
      PRAGMA create_fts_index(
        'vectors',
        'id',
        'page_content',
        overwrite = 1
      );")
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
  if (ncol(q_emb) != embedding_dim)
    stop("Query embedding dimension mismatch.")
  storage.mode(q_emb) <- "double"

  DBI::dbExecute(con, "DROP TABLE IF EXISTS __temp_query__;")
  DBI::dbExecute(con, sprintf(
    "CREATE TEMP TABLE __temp_query__(embedding FLOAT[%d]);", embedding_dim))
  DBI::dbExecute(con, sprintf(
    "INSERT INTO __temp_query__ VALUES (list_value(%s));",
    paste(q_emb[1, ], collapse = ",")))  # list_value works for both column types

  res <- DBI::dbGetQuery(con, sprintf("
    SELECT v.id, v.page_content,
           v.embedding <=> (SELECT embedding FROM __temp_query__) AS dist
    FROM vectors v
    ORDER BY dist ASC
    LIMIT %d;", top_k))

  DBI::dbExecute(con, "DROP TABLE IF EXISTS __temp_query__;")
  res
}
