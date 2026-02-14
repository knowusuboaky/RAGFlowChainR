# create_rag_chain.R Overview

A refined implementation of a LangChain-style Retrieval-Augmented
Generation (RAG) pipeline. Includes vector search across multiple
backends, optional web search using the Tavily API, and a built-in chat
message history.

This function powers \`create_rag_chain()\`, the exported entry point
for constructing a full RAG pipeline.

\## Features: - Context-aware reformulation of user queries - Semantic
chunk retrieval using DuckDB, VectrixDB, Qdrant, Pinecone, Weaviate, or
Elasticsearch - Optional real-time web search (Tavily) - Compatible with
any LLM function (OpenAI, Claude, etc.)

\## Required Packages
`install.packages(c("DBI", "duckdb", "httr", "jsonlite", "stringi", "dplyr"))`

## Arguments

- llm:

  A function that takes a prompt and returns a response (e.g. a call to
  OpenAI or Claude).

- vector_database_directory:

  Path to the vector backend. For \`method = "DuckDB"\`, pass a DuckDB
  database file path. For \`method = "VectrixDB"\`, pass a VectrixDB
  collection path/root path or collection name. For \`method =
  "Qdrant"\`, pass \`"https://host:6333\|collection_name"\`. For
  \`method = "Pinecone"\`, pass \`"https://index-host\|namespace"\`
  (namespace optional). For \`method = "Weaviate"\`, pass
  \`"https://weaviate-host\|ClassName"\`. For \`method =
  "Elasticsearch"\`, pass
  \`"https://elastic-host:9200\|index_name\|vector_field"\` (vector
  field optional).

- method:

  Retrieval backend. One of \`"DuckDB"\`, \`"VectrixDB"\`, \`"Qdrant"\`,
  \`"Pinecone"\`, \`"Weaviate"\`, or \`"Elasticsearch"\`.

- embedding_function:

  A function to embed text. Defaults to
  [`embed_openai()`](https://knowusuboaky.github.io/RAGFlowChainR/reference/create_vectorstore.md).

- system_prompt:

  Optional prompt with placeholders `{chat_history}`, `{input}`,
  `{context}`.

- chat_history_prompt:

  Prompt used to rephrase follow-up questions using prior conversation
  history.

- tavily_search:

  Tavily API key (set to `NULL` to disable web search).

- embedding_dim:

  Integer; embedding vector dimension. Defaults to `1536`.

- use_web_search:

  Logical; whether to include web results from Tavily. Defaults to
  `TRUE`.

## Value

A list of utility functions:

- `invoke(text)` — Performs full context retrieval and LLM response

- `custom_invoke(text)` — Retrieves context only (no LLM call)

- `get_session_history()` — Returns complete conversation history

- `clear_history()` — Clears in-memory chat history

- `disconnect()` — Closes any open local backend connection

## Details

Create a Retrieval-Augmented Generation (RAG) Chain

Creates a LangChain-style RAG chain using DuckDB for vector store
operations, optional Tavily API for web search, and in-memory message
history for conversational context.

## Note

Only `create_rag_chain()` is exported. Helper functions are internal.

## Examples

``` r
if (FALSE) { # \dontrun{
rag_chain <- create_rag_chain(
  llm = call_llm,
  vector_database_directory = "tests/testthat/test-data/my_vectors.duckdb",
  method = "DuckDB",
  embedding_function = embed_openai(),
  use_web_search = FALSE
)

response <- rag_chain$invoke("Tell me about R")
} # }
```
