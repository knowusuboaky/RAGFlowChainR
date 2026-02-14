# Embed text with OpenAI

Helper for vector-store pipelines. If called without \`x\`, this returns
a closure that can be passed directly to \`insert_vectors(embed_fun =
...)\`.

Initializes a DuckDB database connection for storing embedded documents,
with optional support for the experimental \`vss\` extension.

Chunks long text rows, generates embeddings when needed, and inserts
\`(page_content, embedding)\` rows into the \`vectors\` table.

Builds HNSW (\`vss\`) and/or full-text (\`fts\`) indexes on the
\`vectors\` table.

Embeds \`query_text\`, computes vector distance against stored
embeddings, and returns the nearest matches.

## Usage

``` r
embed_openai(
  x,
  model = "text-embedding-ada-002",
  base_url = "https://api.openai.com/v1",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  batch_size = 20L,
  embedding_dim = 1536
)

insert_vectors(
  con,
  df,
  embed_fun = embed_openai(),
  chunk_chars = 12000,
  embedding_dim = 1536
)

build_vector_index(store, type = c("vss", "fts"))

search_vectors(
  con,
  query_text,
  top_k = 5,
  embed_fun = embed_openai(),
  embedding_dim = 1536
)
```

## Arguments

- x:

  Character vector of texts, or a data frame with a \`page_content\`
  column.

- model:

  OpenAI embedding model name.

- base_url:

  Base URL for an OpenAI-compatible API.

- api_key:

  API key; defaults to \`Sys.getenv("OPENAI_API_KEY")\`.

- batch_size:

  Batch size for embedding requests.

- embedding_dim:

  Integer; the dimensionality of the vector embeddings to store.

- con:

  Active DuckDB DBI connection.

- df:

  Data frame containing \`page_content\` (or \`content\`) text.

- embed_fun:

  Function used to convert text into numeric embeddings.

- chunk_chars:

  Approximate max chunk size in bytes before splitting.

- store:

  Active DuckDB DBI connection or vector-store handle.

- type:

  Index types to build; any of \`"vss"\` and/or \`"fts"\`.

- query_text:

  Query text to embed and search.

- top_k:

  Number of nearest matches to return.

- db_path:

  Path to the DuckDB file. Use \`":memory:"\` to create an in-memory
  database.

- overwrite:

  Logical; if \`TRUE\`, deletes any existing DuckDB file or table.

- load_vss:

  Logical; whether to load the experimental \`vss\` extension. This
  defaults to \`TRUE\`, but is forced to \`FALSE\` during CRAN checks.

## Value

For character input, a numeric matrix of embeddings. For data-frame
input, the same data frame with an added \`embedding\` column. If \`x\`
is missing, a configured embedding function is returned.

A live DuckDB connection object. Be sure to manually disconnect with:
`DBI::dbDisconnect(con, shutdown = TRUE)`

## Details

This function is part of the vector-store utilities for:

- Embedding text via the OpenAI API

- Storing and chunking documents in DuckDB

- Building \`HNSW\` and \`FTS\` indexes

- Running nearest-neighbour search over vector embeddings

Core helpers like `embed_openai()`, `insert_vectors()`,
`build_vector_index()`, and `search_vectors()` are also exported to
support composable workflows.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create vector store
con <- create_vectorstore("tests/testthat/test-data/my_vectors.duckdb", overwrite = TRUE)

# Assume response is output from fetch_data()
docs <- data.frame(head(response))

# Insert documents with embeddings
insert_vectors(
  con = con,
  df = docs,
  embed_fun = embed_openai(),
  chunk_chars = 12000
)

# Build vector + FTS indexes
build_vector_index(con, type = c("vss", "fts"))

# Perform vector search
response <- search_vectors(con, query_text = "Tell me about R?", top_k = 5)
} # }
```
