# RAG with VectrixDB Backend

This vignette shows how to use `VectrixDB` as the retrieval backend in
`RAGFlowChainR`.

## 1) Install and load packages

``` r
# install.packages("remotes")
# remotes::install_github("knowusuboaky/vectrixdb-r")

library(RAGFlowChainR)
library(VectrixDB)
```

## 2) Optional API key for dashboard writes

``` r
# Optional: protects write endpoints when using the dashboard/server API.
# Keep this key private and load from your environment in real projects.
Sys.setenv(VECTRIX_API_KEY = "your-vectrix-api-key")
```

## 3) Create a VectrixDB collection and add documents

``` r
store_path <- tempfile("vectrix_store_")
dir.create(store_path, recursive = TRUE, showWarnings = FALSE)

db <- VectrixDB::vectrix_open(name = "default", path = store_path)

db$add(
  texts = c(
    "R is widely used for statistics and data analysis.",
    "VectrixDB is a lightweight vector database implemented in R.",
    "RAGFlowChainR can retrieve context before calling the LLM."
  ),
  metadata = list(
    list(source = "doc_1"),
    list(source = "doc_2"),
    list(source = "doc_3")
  )
)

db$close()
```

## 4) Optional: launch dashboard on port 7377

``` r
# Starts the dashboard at:
#   http://127.0.0.1:7377/dashboard
# Stop with Esc or by interrupting the R session.
VectrixDB::vdb_dashboard(
  data_path = store_path,
  host = "127.0.0.1",
  port = 7377,
  api_key = Sys.getenv("VECTRIX_API_KEY"),
  launch.browser = TRUE
)
```

## 5) Create a RAG chain with method = “VectrixDB”

``` r
mock_llm <- function(prompt) {
  "This is a mock answer from the LLM."
}

rag_chain <- create_rag_chain(
  llm = mock_llm,
  vector_database_directory = store_path,
  method = "VectrixDB",
  use_web_search = FALSE
)
```

## 6) Query the chain

``` r
result <- rag_chain$invoke("What is VectrixDB?")

result$answer
result$documents
```

## 7) Cleanup

``` r
rag_chain$disconnect()
unlink(store_path, recursive = TRUE)
```

## Notes

- `create_rag_chain(method = "VectrixDB")` requires package `VectrixDB`.
- If `VectrixDB` is not installed,
  [`create_rag_chain()`](https://knowusuboaky.github.io/RAGFlowChainR/reference/create_rag_chain.md)
  raises a clear error with installation steps.
- `vector_database_directory` can point to a Vectrix root directory,
  collection directory, or collection name.
- `VectrixDB::vdb_dashboard()` serves the dashboard at port `7377` by
  default.
- `api_key` is optional but recommended for dashboard/API write
  protection.
