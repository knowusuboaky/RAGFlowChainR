# RAG with Elasticsearch Backend

This vignette shows how to query an Elasticsearch vector index from
`RAGFlowChainR`.

## 1) Load packages

``` r
library(RAGFlowChainR)
```

## 2) Configure Elasticsearch target

``` r
# Optional if your cluster requires API key auth.
# Provide either raw key or full "ApiKey <token>" string.
Sys.setenv(ELASTIC_API_KEY = "your-elastic-api-key")

# Format: "https://host:9200|index_name|vector_field"
# vector_field is optional; default is "embedding"
elastic_target <- "https://your-elastic-host:9200|vectors|embedding"
```

## 3) Create the RAG chain

``` r
my_llm <- function(prompt) {
  "Mock answer from LLM."
}

rag_chain <- create_rag_chain(
  llm = my_llm,
  vector_database_directory = elastic_target,
  method = "Elasticsearch",
  embedding_function = embed_openai(model = "text-embedding-3-small"),
  embedding_dim = 1536,
  use_web_search = FALSE
)
```

## 4) Query

``` r
result <- rag_chain$invoke("What are the key points from this index?")
result$answer
result$documents
```

## Notes

- Elasticsearch ingestion/mapping setup happens outside `RAGFlowChainR`.
- Confirm the configured vector field and embedding dimension match your
  index mapping.
