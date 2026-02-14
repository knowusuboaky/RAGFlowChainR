# RAG with DuckDB Backend

This vignette shows a complete `DuckDB` workflow for `RAGFlowChainR`:

1.  Create a local vector store
2.  Insert text chunks and build indexes
3.  Query with
    [`create_rag_chain()`](https://knowusuboaky.github.io/RAGFlowChainR/reference/create_rag_chain.md)

## 1) Load packages

``` r
library(RAGFlowChainR)
library(DBI)
```

## 2) Build a small local DuckDB vector store

``` r
# Tiny deterministic embedder for demos (8 dimensions).
toy_embed <- function(x, embedding_dim = 8) {
  if (is.data.frame(x) && "page_content" %in% names(x)) {
    x$embedding <- toy_embed(x$page_content, embedding_dim = embedding_dim)
    return(x)
  }
  if (!length(x)) {
    return(matrix(numeric(0), nrow = 0, ncol = embedding_dim))
  }
  set.seed(42)
  matrix(runif(length(x) * embedding_dim), nrow = length(x), ncol = embedding_dim)
}

db_path <- tempfile(fileext = ".duckdb")

con <- create_vectorstore(
  db_path = db_path,
  overwrite = TRUE,
  embedding_dim = 8
)

docs <- data.frame(
  page_content = c(
    "R is a language for statistics and data science.",
    "DuckDB is an in-process analytical database.",
    "RAG combines retrieval and generation for grounded answers."
  ),
  stringsAsFactors = FALSE
)

insert_vectors(
  con = con,
  df = docs,
  embed_fun = toy_embed,
  embedding_dim = 8
)

build_vector_index(con)
DBI::dbDisconnect(con)
```

## 3) Create a DuckDB RAG chain

``` r
mock_llm <- function(prompt) {
  "This is a mock answer from the LLM."
}

rag_chain <- create_rag_chain(
  llm = mock_llm,
  vector_database_directory = db_path,
  method = "DuckDB",
  embedding_function = toy_embed,
  embedding_dim = 8,
  use_web_search = FALSE
)
```

## 4) Query the chain

``` r
result <- rag_chain$invoke("What is DuckDB?")

result$answer
result$documents
```

## 5) Cleanup

``` r
rag_chain$disconnect()
unlink(db_path)
```

## Notes

- Use your real LLM wrapper instead of `mock_llm`.
- For production, use a real embedding function (for example
  [`embed_openai()`](https://knowusuboaky.github.io/RAGFlowChainR/reference/create_vectorstore.md)).
- Keep `embedding_dim` consistent between insert and retrieval.
