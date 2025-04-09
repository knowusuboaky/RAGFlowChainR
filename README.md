# RAGFlowChainR

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/RAGFlowChainR)](https://cran.r-project.org/package=RAGFlowChainR)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/RAGFlowChainR?color=orange)](https://cranlogs.r-pkg.org/badges/grand-total/RAGFlowChainR)
[![Codecov test coverage](https://codecov.io/gh/knowusuboaky/RAGFlowChainR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/knowusuboaky/RAGFlowChainR?branch=main)
[![Last Commit](https://img.shields.io/github/last-commit/knowusuboaky/RAGFlowChainR.svg)](https://github.com/knowusuboaky/RAGFlowChainR/commits/main)
[![Issues](https://img.shields.io/github/issues/knowusuboaky/RAGFlowChainR.svg)](https://github.com/knowusuboaky/RAGFlowChainR/issues)
<!-- badges: end -->


## Overview

**RAGFlowChainR** is an R package that brings Retrieval-Augmented Generation (RAG) capabilities to R, inspired by LangChain. It enables intelligent retrieval of documents from a local vector store (DuckDB), enhanced with optional web search, and seamless integration with Large Language Models (LLMs).

Features include:

- 📂 Intelligent document ingestion from local files and websites.
- 🔍 Semantic search powered by vector embeddings (OpenAI or Ollama).
- 🧠 RAG chain execution with conversational memory and dynamic prompt construction.
- 🔌 Extensible architecture for embedding, indexing, and invoking LLMs.

For the Python version, see: [RAGFlowChain (PyPI)](https://pypi.org/project/RAGFlowChain/0.5.1/)  
🔗 GitHub (R): [RAGFlowChainR](https://github.com/knowusuboaky/RAGFlowChainR)  
🔗 GitHub (Python): [RAGFlowChain](https://github.com/knowusuboaky/RAGFlowChain)

---

## Installation

```r
# Install from GitHub
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("knowusuboaky/RAGFlowChainR")
```
---

## 🔐 Environment Setup

To use features like web search (`Tavily`) and LLMs (`OpenAI`, `Groq`, `Anthropic`), you’ll need to set up your API keys as environment variables. This ensures that sensitive credentials are **never hardcoded** in your scripts.

### Example: Setting Environment Variables in R

```r
# Add these to your .Renviron file or run once per session
Sys.setenv(TAVILY_API_KEY = "your-tavily-api-key")
Sys.setenv(OPENAI_API_KEY = "your-openai-api-key")
Sys.setenv(GROQ_API_KEY = "your-groq-api-key")
Sys.setenv(ANTHROPIC_API_KEY = "your-anthropic-api-key")
```

> 💡 Tip: To persist these keys across sessions, add them to a `~/.Renviron` file (not tracked by git) instead of your code.

### .Renviron Example

Place this in a file named `.Renviron` in your home directory:

```
TAVILY_API_KEY=your-tavily-api-key
OPENAI_API_KEY=your-openai-api-key
GROQ_API_KEY=your-groq-api-key
ANTHROPIC_API_KEY=your-anthropic-api-key
```

Then restart R for the changes to take effect.

---

## Usage

### 1. Data Ingestion with `fetch_data()`

```r
library(RAGFlowChainR)

# Read local files and websites
local_files <- c("documents/sample.pdf", "documents/sample.txt")
website_urls <- c("https://www.r-project.org")
crawl_depth <- 1

data <- fetch_data(local_paths = local_files, website_urls = website_urls, crawl_depth = crawl_depth)
head(data)
```

### 2. Creating and Using a Vector Store

```r
con <- create_vectorstore("my_vectors.duckdb", overwrite = TRUE)

docs <- data.frame(
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

insert_vectors(
  con = con,
  df = docs,
  embed_fun = embed_openai(),  # Or embed_ollama()
  chunk_chars = 12000
)

build_vector_index(con, type = c("vss", "fts"))

results <- search_vectors(con, query_text = "Who is Messi?", top_k = 5)
print(results)
dbDisconnect(con)
```

### 3. Using a RAG Chain

```r
rag_chain <- create_rag_chain(
  llm = call_llm,
  vector_database_directory = "my_vectors.duckdb",
  method = "DuckDB",
  embedding_function = embed_openai(),
  use_web_search = FALSE
)

# Ask a question
response <- rag_chain$invoke("Tell me about Messi")
cat(response$answer)

# Get related documents
context <- rag_chain$custom_invoke("Tell me about Messi")
print(context$documents)

# Review and clear chat history
print(rag_chain$get_session_history())
rag_chain$clear_history()
rag_chain$disconnect()
```

---

## LLM Support

RAGFlowChainR includes built-in support for calling LLMs from providers such as **OpenAI**, **Groq**, and **Anthropic** via the `call_llm()` utility:

```r
call_llm(
  prompt = "Summarize the capital of France.",
  provider = "groq",
  model = "llama3-8b",
  temperature = 0.7,
  max_tokens = 200
)
```

---

### 🔧 Coming Soon: [`chatLLM`](https://github.com/knowusuboaky/chatLLM)

We’re developing a standalone R package, **`chatLLM`**, that will offer a unified, modular interface for interacting with popular LLM providers—**OpenAI**, **Groq**, and **Anthropic**—via a clean, extensible API.

Features planned:

- 🔁 Seamless provider switching (`openai`, `groq`, `anthropic`)
- ✍️ Prompt and system message templating
- 🚀 Support for streaming responses (planned)
- 🔌 Native integration with `RAGFlowChainR`
- 🔐 Flexible environment-based or direct API key support

Stay tuned on [GitHub](https://github.com/knowusuboaky/chatLLM) for updates!

---

## License

MIT © [Kwadwo Daddy Nyame Owusu Boakye](mailto:kwadwo.owusuboakye@outlook.com)
