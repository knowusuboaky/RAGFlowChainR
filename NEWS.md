# RAGFlowChainR (development version)

* No changes yet.

# RAGFlowChainR 0.1.4

* Added four new retrieval backends in `create_rag_chain()`:
  * `Qdrant`
  * `Pinecone`
  * `Weaviate`
  * `Elasticsearch`
* Expanded backend routing and parsing in `create_rag_chain()` / `perform_web_search()` to support:
  * Local backends: `DuckDB`, `VectrixDB`
  * Remote backends: `Qdrant`, `Pinecone`, `Weaviate`, `Elasticsearch`
* Added backend adapter tests for all new methods and kept existing `DuckDB`/`VectrixDB` coverage.
* Added backend-focused vignettes:
  * `duckdb-backend.Rmd`
  * `vectrixdb-backend.Rmd`
  * `qdrant-backend.Rmd`
  * `pinecone-backend.Rmd`
  * `weaviate-backend.Rmd`
  * `elasticsearch-backend.Rmd`
* Updated `_pkgdown.yml` backend articles grouping:
  * `Requires Embedding ML`
  * `Doesnt Require Embedding ML`
* CI/workflow improvements:
  * Added `.github/workflows/R-CMD-check.yaml`
  * Added `.github/workflows/rmd-check.yaml`
  * Updated `pkgdown.yaml` to run on `main` and deploy with clean pages output.
* README updates:
  * Streamlined README content and moved detailed backend walkthroughs to vignettes.
  * Added backend guide links for faster navigation.

* Wrapped FTS-related tests in `tryCatch()` + `skip()` to avoid segmentation faults on Fedora-clang during CRAN checks.
* Informative `skip()` messages added for systems without FTS extension support.
* Ensured all tests pass cleanly on platforms with partial DuckDB extension support.
* Maintained full feature test coverage in interactive/development environments.

# RAGFlowChainR 0.1.1

* Prevented segfaults on CRAN by disabling the experimental `vss` extension during checks.
* Fallback to `FLOAT[]` column type when `vss` is unavailable, avoiding Fedora-clang binary-extension errors.

* `create_vectorstore()` now:
  * Skips `vss` installation/loading under CRAN but retains support in dev environments.
  * Returns a persistent DuckDB connection (no longer auto-disconnects).

* `insert_vectors()`:
  * Handles single-column frames safely using `drop = FALSE`.
  * Dynamically switches between `array_value()` and `list_value()` based on schema.

* `build_vector_index()`:
  * Skips HNSW index creation when `VECTOR[]` columns are missing (with a warning).

* RAG chain integration:
  * Safely mirrors `vss`-guard logic in `connect_vectorstore()` and `create_rag_chain()`.

* DESCRIPTION and metadata:
  * Set minimum required version: `duckdb (>= 0.10.0)` and `testthat (>= 3.0.0)`.
  * Added `Config/testthat/edition: 3` for consistent test behavior.
  * Tidied Description field by removing unnecessary single quotes around names.
  * Expanded common acronyms like `HNSW`, `LLM`, etc.

* Testing improvements:
  * Suppressed non-critical test warnings.
  * Removed fragile FTS assertions (`fts_main`).
  * Introduced `mock_embed()` and `dummy_embed()` to remove dependency on external APIs.
  * Skipped RAG-chain integration tests on CRAN and gated test-data downloads.
