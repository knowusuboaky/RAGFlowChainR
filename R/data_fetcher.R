#' @title data_fetcher.R Overview
#' @description
#' Provides the `fetch_data()` function, which extracts and structures content from:
#' \itemize{
#'   \item Local files (PDF, DOCX, PPTX, TXT, HTML)
#'   \item Crawled websites (with optional BFS crawl depth)
#' }
#'
#' The returned data frame includes metadata columns like `title`, `author`, `publishedDate`,
#' and the main extracted `content`.
#'
#' ## Required Packages
#' \code{install.packages(c("pdftools", "officer", "rvest", "xml2", "dplyr", "stringi", "curl", "httr", "jsonlite", "magrittr"))}
#'
#' @note Only `fetch_data()` is exported. Internal functions include `read_local_file()`, `read_website_page()`, and `crawl_links_bfs()`.
#' @name data_fetcher
NULL

# Required libraries
library(pdftools)
library(officer)
library(rvest)
library(xml2)
library(curl)
library(dplyr)
library(stringi)
library(httr)
library(jsonlite)
library(magrittr)

# ------------------------------------------------------------------------------
#  1) Local file reading
# ------------------------------------------------------------------------------
read_local_file <- function(file_path) {
    ext <- tolower(tools::file_ext(file_path))
    if (!file.exists(file_path)) {
        message("File not found: ", file_path)
        return(NULL)
    }

    message("Processing file: ", file_path)

    tryCatch({
        if (ext == "pdf") {
            info <- pdftools::pdf_info(file_path)
            txt_vec <- pdftools::pdf_text(file_path)
            combined <- paste(txt_vec, collapse = "\n")
            combined <- gsub("\n\n+", "\n", combined)
            data.frame(
                source        = file_path,
                title         = ifelse(!is.null(info$title) && nzchar(info$title), info$title, NA_character_),
                author        = ifelse(!is.null(info$author) && nzchar(info$author), info$author, NA_character_),
                publishedDate = ifelse(!is.null(info$created), as.character(info$created), NA_character_),
                description   = NA_character_,
                content       = combined,
                url           = NA_character_,
                source_type   = "pdf",
                stringsAsFactors = FALSE
            )

        } else if (ext == "docx") {
            doc <- officer::read_docx(file_path)
            props <- officer::doc_properties(doc)
            doc_sum <- officer::docx_summary(doc)
            combined <- paste(doc_sum$text, collapse = "\n")
            combined <- gsub("\n\n+", "\n", combined)
            data.frame(
                source        = file_path,
                title         = ifelse(!is.null(props$title) && nzchar(props$title), props$title, NA_character_),
                author        = ifelse(!is.null(props$author) && nzchar(props$author), props$author, NA_character_),
                publishedDate = ifelse(!is.null(props$created), as.character(props$created), NA_character_),
                description   = NA_character_,
                content       = combined,
                url           = NA_character_,
                source_type   = "docx",
                stringsAsFactors = FALSE
            )

        } else if (ext == "pptx") {
            ppt <- officer::read_pptx(file_path)
            props <- officer::doc_properties(ppt)
            slide_info <- officer::pptx_summary(ppt)
            combined <- paste(slide_info$text, collapse = "\n")
            combined <- gsub("\n\n+", "\n", combined)
            data.frame(
                source        = file_path,
                title         = ifelse(!is.null(props$title) && nzchar(props$title), props$title, NA_character_),
                author        = ifelse(!is.null(props$author) && nzchar(props$author), props$author, NA_character_),
                publishedDate = ifelse(!is.null(props$created), as.character(props$created), NA_character_),
                description   = NA_character_,
                content       = combined,
                url           = NA_character_,
                source_type   = "pptx",
                stringsAsFactors = FALSE
            )

        } else if (ext == "txt") {
            lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
            combined <- paste(lines, collapse = "\n")
            combined <- gsub("\n\n+", "\n", combined)
            data.frame(
                source        = file_path,
                title         = NA_character_,
                author        = NA_character_,
                publishedDate = NA_character_,
                description   = NA_character_,
                content       = combined,
                url           = NA_character_,
                source_type   = "txt",
                stringsAsFactors = FALSE
            )

        } else if (ext %in% c("html", "htm")) {
            doc <- xml2::read_html(file_path)
            raw_text <- rvest::html_text2(doc)
            raw_text <- gsub("\n\n+", "\n", raw_text)
            page_title <- rvest::html_element(doc, "title") |> rvest::html_text(trim = TRUE)
            meta <- rvest::html_elements(doc, "meta")
            description <- rvest::html_attr(meta[rvest::html_attr(meta, "name") == "description"], "content")
            author <- rvest::html_attr(meta[rvest::html_attr(meta, "name") == "author"], "content")

            data.frame(
                source        = file_path,
                title         = ifelse(!is.na(page_title) && nzchar(page_title), page_title, NA_character_),
                author        = ifelse(length(author) > 0 && nzchar(author[1]), author[1], NA_character_),
                publishedDate = NA_character_,
                description   = ifelse(length(description) > 0 && nzchar(description[1]), description[1], NA_character_),
                content       = raw_text,
                url           = NA_character_,
                source_type   = "html",
                stringsAsFactors = FALSE
            )

        } else {
            message("Skipping unsupported extension: ", ext)
            NULL
        }
    }, error = function(e) {
        message("Error processing ", file_path, ": ", conditionMessage(e))
        NULL
    })
}

read_local_files <- function(paths) {
    all_dfs <- list()
    for (p in paths) {
        if (dir.exists(p)) {
            all_files <- list.files(p, recursive = TRUE, full.names = TRUE)
            message("Found ", length(all_files), " files in directory: ", p)
            for (f in all_files) {
                tmp <- read_local_file(f)
                if (!is.null(tmp)) all_dfs[[length(all_dfs) + 1]] <- tmp
            }
        } else if (file.exists(p)) {
            tmp <- read_local_file(p)
            if (!is.null(tmp)) all_dfs[[length(all_dfs) + 1]] <- tmp
        } else {
            message("Path not found: ", p)
        }
    }

    if (!length(all_dfs)) {
        message("No valid files were processed")
        return(data.frame(
            source = character(), title = character(), author = character(),
            publishedDate = character(), description = character(), content = character(),
            url = character(), source_type = character()
        ))
    }

    dplyr::bind_rows(all_dfs)
}

# ------------------------------------------------------------------------------
#  2) BFS Link Crawl with optional numeric depth or infinite if depth=NULL
# ------------------------------------------------------------------------------
crawl_links_bfs <- function(start_url, depth = NULL) {
    visited <- character()
    queue <- list(list(url = start_url, d = 0L))
    main_host <- tryCatch(curl::curl_parse_url(start_url)$host, error = function(e) "")
    discovered <- character()

    while (length(queue)) {
        item <- queue[[1]]; queue <- queue[-1]
        cur_url <- item$url; cur_d <- item$d
        if (cur_url %in% visited) next
        visited <- c(visited, cur_url)

        doc <- tryCatch(xml2::read_html(cur_url), error = function(e) NULL)
        if (is.null(doc)) next

        anchors <- rvest::html_elements(doc, "a[href]")
        links <- rvest::html_attr(anchors, "href")
        links <- links[!is.na(links)]
        links <- sub("#.*$", "", links)
        links <- unique(links)

        abs_links <- sapply(links, function(x) {
            tryCatch(xml2::url_absolute(x, cur_url), error = function(e) x)
        }, USE.NAMES = FALSE)

        keep <- sapply(abs_links, function(x) {
            tryCatch(curl::curl_parse_url(x)$host, error = function(e) "") == main_host
        })
        abs_links <- abs_links[keep]
        discovered <- union(discovered, abs_links)

        can_go_deeper <- is.null(depth) || (cur_d < depth)
        if (can_go_deeper) {
            new_links <- setdiff(abs_links, visited)
            for (lnk in new_links) {
                queue[[length(queue) + 1]] <- list(url = lnk, d = cur_d + 1L)
            }
        }
    }

    c(start_url, discovered)
}

# ------------------------------------------------------------------------------
#  3) Read a single site
# ------------------------------------------------------------------------------
read_website_page <- function(url) {
    doc <- tryCatch(xml2::read_html(url), error = function(e) NULL)
    if (is.null(doc)) return(NULL)
    raw_text <- rvest::html_text2(doc)
    raw_text <- gsub("\n\n+", "\n", raw_text)
    page_title <- rvest::html_element(doc, "title") |> rvest::html_text(trim = TRUE)
    if (is.na(page_title) || !nzchar(page_title)) page_title <- NA_character_

    data.frame(
        source = url, title = page_title, author = NA_character_,
        publishedDate = NA_character_, description = NA_character_,
        content = raw_text, url = url, source_type = "website",
        stringsAsFactors = FALSE
    )
}

# ------------------------------------------------------------------------------
#  4) Main fetch_data()
# ------------------------------------------------------------------------------
#' Fetch Data from Local Files and Websites
#'
#' Extracts content and metadata from local documents or websites. Supports PDF, DOCX, PPTX, TXT, HTML files
#' and performs BFS web crawling up to the specified depth.
#'
#' @param local_paths A character vector of file paths or directories to scan for documents.
#' @param website_urls A character vector of website URLs to crawl and extract text from.
#' @param crawl_depth Integer indicating BFS crawl depth; set to NULL for infinite crawl.
#'
#' @return A data frame with the following columns: source, title, author, publishedDate, description, content, url, source_type.
#'
#' @export
fetch_data <- function(local_paths = NULL, website_urls = NULL, crawl_depth = NULL) {
    all_dfs <- list()

    if (!is.null(local_paths)) {
        df_local <- read_local_files(local_paths)
        if (nrow(df_local)) all_dfs[[length(all_dfs) + 1]] <- df_local
    }

    if (!is.null(website_urls)) {
        all_links <- character()
        for (u in website_urls) {
            found <- crawl_links_bfs(u, depth = crawl_depth)
            all_links <- union(all_links, found)
        }
        website_list <- lapply(all_links, read_website_page)
        website_list <- website_list[!sapply(website_list, is.null)]
        if (length(website_list)) {
            df_web <- dplyr::bind_rows(website_list)
            all_dfs[[length(all_dfs) + 1]] <- df_web
        }
    }

    if (!length(all_dfs)) {
        return(data.frame(
            source = character(), title = character(), author = character(),
            publishedDate = character(), description = character(), content = character(),
            url = character(), source_type = character()
        ))
    }

    final_df <- dplyr::bind_rows(all_dfs)
    col_order <- c("source", "title", "author", "publishedDate", "description", "content", "url", "source_type")
    final_df <- final_df[, col_order, drop = FALSE]
    final_df
}
