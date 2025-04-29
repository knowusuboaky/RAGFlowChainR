# RAGFlowChainR (development version)

* Wrapped FTS-related tests in `tryCatch()` + `skip()` to avoid segmentation faults on Fedora-clang during CRAN checks.
* Informative `skip()` messages added for systems without FTS extension support.
* Ensured all tests pass cleanly on platforms with partial DuckDB extension support.
* Maintained full feature test coverage in interactive/development environments.
* No user-facing changes or exported API modifications.

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
