# RAGFlowChainR NEWS

All notable changes to **RAGFlowChainR** will be documented in this file.

## Version 0.1.1 — 2025-04-24

### CRAN compliance & stability
- **Prevent segfaults on CRAN** by disabling the experimental `vss` extension during CRAN checks (no more `INSTALL vss; LOAD vss;` in tests or `create_vectorstore()` when under `_R_CHECK_PACKAGE_NAME_`).
- **Fallback to `FLOAT[]`** column type when `vss` is unavailable, avoiding binary-extension errors on Fedora-clang.

### API fixes & enhancements
- **`create_vectorstore()`**  
  - Skip installing/loading `vss` under CRAN, but still load in interactive/developer sessions.
  - Always return a live DuckDB connection (no longer auto-disconnect internally).
- **`insert_vectors()`**  
  - Fix single-column data-frame collapse by using `df[i, , drop = FALSE]`.
  - Dynamically choose between `array_value()` (for `VECTOR[…]`) and `list_value()` (for `FLOAT[]`) constructors.
- **`build_vector_index()`**  
  - Guard HNSW index creation: skip with a warning if the table uses `FLOAT[]` (no `VECTOR[…]` column).
- **`connect_vectorstore()` / RAG chain code**  
  - Mirror the same safe `vss`-guard logic when connecting and building indexes in `create_rag_chain()`.

### DESCRIPTION & metadata
- Bump **Version** to **0.1.1**.
- Require **duckdb (>= 0.10.0)** to ensure extension support.
- Require **testthat (>= 3.0.0)** and add `Config/testthat/edition: 3`.
- Clean up single quotes in **Description** (only around package/API names: `DuckDB`, `Tavily`, `python`).
- Expand acronyms: `HNSW (Hierarchical Navigable Small World)`, `large language model (LLM)`.

### Testing improvements
- **Suppress warnings** in tests to achieve silent runs.
- Remove brittle 'fts_main' table assertions—only verify `build_vector_index()` completes without error.
- Use deterministic mock_embed and dummy_embed functions to avoid external API calls.
- Skip RAG-chain integration tests on CRAN and guard downloads of test-data DuckDB file.

---

*Thanks to the CRAN team for the review and feedback!*