# RAG with Qdrant Backend

This vignette shows how to query an existing Qdrant collection with
`RAGFlowChainR`.

## 1) Load packages

``` r
library(RAGFlowChainR)
```

## 2) Configure Qdrant target

``` r
# Optional if your Qdrant deployment requires API key auth
Sys.setenv(QDRANT_API_KEY = "your-qdrant-api-key")

# Format: "https://host:6333|collection_name"
qdrant_target <- "https://your-qdrant-host:6333|my_collection"
```

## 3) Create the RAG chain

``` r
my_llm <- function(prompt) {
  "Mock answer from LLM."
}

rag_chain <- create_rag_chain(
  llm = my_llm,
  vector_database_directory = qdrant_target,
  method = "Qdrant",
  embedding_function = embed_openai(model = "text-embedding-3-small"),
  embedding_dim = 1536,
  use_web_search = FALSE
)
```

## 4) Query

``` r
result <- rag_chain$invoke("What does this collection say about RAG?")
result$answer
result$documents
```

## Notes

- Qdrant ingestion is managed outside `RAGFlowChainR`.
- Ensure your embedding model and dimension match the vectors stored in
  Qdrant.
