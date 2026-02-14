# Fetch data from local files and websites

Extracts content and metadata from local documents or websites.
Supports:

- Local files: PDF, DOCX, PPTX, TXT, HTML

- Crawled websites: with optional breadth-first crawl depth

## Arguments

- local_paths:

  A character vector of file paths or directories to scan for documents.

- website_urls:

  A character vector of website URLs to crawl and extract text from.

- crawl_depth:

  Integer indicating BFS crawl depth; use `NULL` for unlimited depth.

## Value

A data frame with extracted metadata and content.

## Details

The returned data frame includes structured columns such as: `source`,
`title`, `author`, `publishedDate`, `description`, `content`, `url`, and
`source_type`.

\## Required Packages
`install.packages(c("pdftools", "officer", "rvest", "xml2", "dplyr", "stringi", "curl", "httr", "jsonlite", "magrittr"))`

## Note

Internal functions used include `read_local_file()`,
`read_website_page()`, and `crawl_links_bfs()`.

## Examples

``` r
if (FALSE) { # \dontrun{
local_files <- c("tests/testthat/test-data/sprint.pdf",
                 "tests/testthat/test-data/introduction.pptx",
                 "tests/testthat/test-data/overview.txt")
website_urls <- c("https://www.r-project.org")
crawl_depth <- 1

response <- fetch_data(
  local_paths = local_files,
  website_urls = website_urls,
  crawl_depth = crawl_depth
)
} # }
```
