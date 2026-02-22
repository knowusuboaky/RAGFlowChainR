
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RAGFlowChainR <a href="https://knowusuboaky.github.io/RAGFlowChainR/"><img src="man/figures/ragopenlogo.png" align="right" height="120" /></a>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/knowusuboaky/RAGFlowChainR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/knowusuboaky/RAGFlowChainR/actions/workflows/R-CMD-check.yaml)
[![Docs](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://knowusuboaky.github.io/RAGFlowChainR/)
[![CRAN
status](https://www.r-pkg.org/badges/version/RAGFlowChainR)](https://cran.r-project.org/package=RAGFlowChainR)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/RAGFlowChainR?color=orange)](https://cranlogs.r-pkg.org/badges/grand-total/RAGFlowChainR)
[![Last
Commit](https://img.shields.io/github/last-commit/knowusuboaky/RAGFlowChainR.svg)](https://github.com/knowusuboaky/RAGFlowChainR/commits/main)
[![Issues](https://img.shields.io/github/issues/knowusuboaky/RAGFlowChainR.svg)](https://github.com/knowusuboaky/RAGFlowChainR/issues)
<!-- badges: end -->

## Overview

`RAGFlowChainR` is an R package for Retrieval-Augmented Generation (RAG)
workflows with local retrieval backends (`DuckDB` and `VectrixDB`) plus
optional web search.

The README is intentionally short. Full backend workflows are documented
in vignettes.

------------------------------------------------------------------------

## Installation

``` r
install.packages("RAGFlowChainR")
```

------------------------------------------------------------------------

## Development version

``` r
install.packages("remotes")
remotes::install_github("knowusuboaky/RAGFlowChainR")
```

------------------------------------------------------------------------

## Backend Guides

- DuckDB backend article:
  <https://knowusuboaky.github.io/RAGFlowChainR/articles/duckdb-backend.html>
- VectrixDB backend article:
  <https://knowusuboaky.github.io/RAGFlowChainR/articles/vectrixdb-backend.html>
- Function reference:
  <https://knowusuboaky.github.io/RAGFlowChainR/reference/>

------------------------------------------------------------------------

## Quick Start

``` r
library(RAGFlowChainR)

rag <- create_rag_chain(
  llm = function(prompt) "mock answer",
  vector_database_directory = "my_vectors.duckdb",
  method = "DuckDB",
  use_web_search = FALSE
)

rag$invoke("What is RAG?")
rag$disconnect()
```

For complete ingestion, indexing, and backend-specific setup, use the
two backend vignettes above.

------------------------------------------------------------------------

## Environment Setup

``` r
Sys.setenv(TAVILY_API_KEY    = "your-tavily-api-key")
Sys.setenv(OPENAI_API_KEY    = "your-openai-api-key")
Sys.setenv(GROQ_API_KEY      = "your-groq-api-key")
Sys.setenv(ANTHROPIC_API_KEY = "your-anthropic-api-key")
```

------------------------------------------------------------------------

## License

MIT (c) [Kwadwo Daddy Nyame Owusu
Boakye](mailto:kwadwo.owusuboakye@outlook.com)
