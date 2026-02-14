# RAG with Pinecone Backend

This vignette shows how to query an existing Pinecone index/namespace
from `RAGFlowChainR`.

## 1) Load packages

``` r
library(RAGFlowChainR)
```

## 2) Configure Pinecone target

``` r
# Optional if your endpoint requires API key auth
Sys.setenv(PINECONE_API_KEY = "your-pinecone-api-key")

# Format: "https://index-host|namespace"
# Namespace can be left empty: "https://index-host|"
pinecone_target <- "https://your-index-host|my_namespace"
```

## 3) Create the RAG chain

``` r
my_llm <- function(prompt) {
  "Mock answer from LLM."
}

rag_chain <- create_rag_chain(
  llm = my_llm,
  vector_database_directory = pinecone_target,
  method = "Pinecone",
  embedding_function = embed_openai(model = "text-embedding-3-small"),
  embedding_dim = 1536,
  use_web_search = FALSE
)
```

## 4) Query

``` r
result <- rag_chain$invoke("What does this Pinecone namespace contain?")
result$answer
result$documents
```

## Notes

- Pinecone indexing/upserts happen outside `RAGFlowChainR`.
- Match `embedding_function` and `embedding_dim` to the vectors stored
  in Pinecone.
