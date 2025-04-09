#' @title create_rag_chain.R Overview
#' @description
#' A refined implementation of a LangChain-style Retrieval-Augmented Generation (RAG) pipeline.
#' Includes vector search using DuckDB, optional web search using the Tavily API, and a
#' built-in chat message history.
#'
#' This script powers `create_rag_chain()`, the exported entry point for constructing a RAG pipeline.
#'
#' ## Features:
#' - Context-aware reformulation of user questions based on chat history
#' - Retrieval of relevant chunks via semantic search
#' - Optional real-time web search using Tavily (if API key is set)
#' - Works with any LLM function (e.g., OpenAI, Claude)
#'
#' ## Required Packages
#' Install with:
#' \code{install.packages(c("DBI", "duckdb", "httr", "jsonlite", "stringi", "dplyr"))}
#'
#' @note Only `create_rag_chain()` is exported.
#' @name create_rag_chain
NULL

# Required libraries
library(DBI)
library(duckdb)
library(httr)
library(jsonlite)
library(stringi)
library(dplyr)

# ==============================================================================
#  1) EMBEDDING FUNCTIONS
# ==============================================================================
embed_openai <- function(
        x,
        model         = "text-embedding-ada-002",
        base_url      = "https://api.openai.com/v1",
        api_key       = Sys.getenv("OPENAI_API_KEY"),
        batch_size    = 20L,
        embedding_dim = 1536
) {
    if (missing(x) || is.null(x)) {
        force(model); force(base_url); force(api_key); force(batch_size); force(embedding_dim)
        return(function(texts) embed_openai(
            texts, model, base_url, api_key, batch_size, embedding_dim
        ))
    }
    if (is.data.frame(x)) {
        x[["embedding"]] <- embed_openai(
            x[["page_content"]], model, base_url, api_key, batch_size, embedding_dim
        )
        return(x)
    }
    if (!nzchar(api_key)) {
        stop("Please set OPENAI_API_KEY in your environment.")
    }
    if (!length(x)) {
        return(matrix(numeric(0), nrow = 0, ncol = embedding_dim))
    }

    body <- list(model = model, input = as.list(x))
    res <- httr::POST(
        url = file.path(base_url, "embeddings"),
        encode = "json",
        body   = body,
        httr::add_headers(Authorization = paste("Bearer", api_key))
    )
    if (httr::http_error(res)) {
        stop("OpenAI API error:\n", httr::content(res, "text"))
    }
    parsed <- httr::content(res, "parsed")
    emb_list <- lapply(parsed$data, function(z) z$embedding)
    emb_mat <- do.call(rbind, emb_list)
    storage.mode(emb_mat) <- "double"
    if (ncol(emb_mat) != embedding_dim) {
        stop(sprintf(
            "OpenAI returned %d-d embeddings, but embedding_dim=%d. Adjust accordingly.",
            ncol(emb_mat), embedding_dim
        ))
    }
    emb_mat
}

# ==============================================================================
#  2) VECTOR DATABASE (DuckDB)
# ==============================================================================
connect_vectorstore <- function(db_path = ":memory:", read_only = FALSE) {
    con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = read_only)
    DBI::dbExecute(con, "INSTALL vss; LOAD vss;")
    con
}

# ==============================================================================
#  3) MESSAGE HISTORY CLASS (Equivalent to SimpleMessageHistory)
# ==============================================================================
SimpleMessageHistory <- function() {
    # Private data: stores messages as a list of lists with role and content.
    messages <- list()
    list(
        add_messages = function(new_messages) {
            messages <<- c(messages, new_messages)
        },
        get_messages = function() {
            messages
        },
        clear_messages = function() {
            messages <<- list()
        }
    )
}

# ==============================================================================
#  4) PROMPT TEMPLATES AND DEFAULTS
# ==============================================================================
# Default system prompt. IMPORTANT: We now explicitly reference the user's question {input}
# and the retrieved context {context}, plus any chat history {chat_history}.
default_system_prompt <- "
You are an assistant for question-answering tasks.
Below is the conversation so far, followed by retrieved context, then the user's latest question.

Conversation history:
{chat_history}

User's question:
{input}

Retrieved context:
{context}

Provide a concise answer (up to 3 sentences). If you don't know, say so.
"

# A system prompt for re-formulating a question that references prior chat. We do NOT want an answer here.
contextualize_q_system_prompt <- "
You are a system for re-formulating user queries. Below is the previous conversation (chat history)
and the user's latest query, which may refer to earlier context.

Chat history:
{chat_history}

User's latest question:
{input}

Reformulate the question so it stands alone and can be understood without the chat history.
Do not answer it; just return the reformulated question.
"

# Helper function to replace placeholders in the prompt template
format_prompt <- function(template, replacements) {
    result <- template
    for (name in names(replacements)) {
        placeholder <- paste0("\\{", name, "\\}")
        result <- gsub(placeholder, replacements[[name]], result)
    }
    result
}

# ==============================================================================
#  5) WEB SEARCH (Tavily API) - Optional
# ==============================================================================
perform_tavily_search <- function(query, tavily_search = NULL, max_results = 5) {
    # Determine the API key (either provided directly as a string or from the environment)
    api_key <- if (is.character(tavily_search)) {
        tavily_search
    } else {
        Sys.getenv("TAVILY_API_KEY")
    }

    if (!nzchar(api_key)) {
        warning("No TAVILY_API_KEY provided. Skipping web search.")
        return(NULL)
    }

    url <- "https://api.tavily.com/search"
    body <- list(
        api_key = api_key,
        query = query,
        max_results = max_results
    )

    res <- httr::POST(
        url = url,
        encode = "json",
        body = body
    )

    if (httr::http_error(res)) {
        warning("Tavily API error: ", httr::content(res, "text"))
        return(NULL)
    }

    parsed <- httr::content(res, "parsed")
    results <- parsed$results
    if (length(results) == 0) {
        return(NULL)
    }

    # Combine results into a single string (customize as desired)
    web_content <- paste(sapply(results, function(r) r$content), collapse = "\n")
    list(page_content = web_content)
}

# This function merges vector DB results with optional web search results
perform_web_search <- function(input_text, con, embed_fun, embedding_dim, tavily_search = NULL, use_web_search = TRUE) {
    # Initialize documents from vector search
    vector_results <- search_vectors(
        con         = con,
        query_text  = input_text,
        top_k       = 5,
        embed_fun   = embed_fun,
        embedding_dim = embedding_dim
    )

    documents <- NULL
    if (nrow(vector_results) > 0) {
        documents <- lapply(1:nrow(vector_results), function(i) {
            list(
                page_content = vector_results$page_content[i],
                metadata = list(
                    id    = vector_results$id[i],
                    dist  = vector_results$dist[i],
                    source = "vector_database"
                )
            )
        })
    }

    # Optionally do Tavily web search
    if (use_web_search) {
        web_result <- perform_tavily_search(input_text, tavily_search = tavily_search)
        if (!is.null(web_result)) {
            Sys.sleep(1)  # optional delay for realism
            if (is.null(documents)) {
                documents <- list(web_result)
            } else {
                documents <- c(documents, list(web_result))
            }
        }
    }

    documents
}

# ==============================================================================
#  6) INSERT WITH CHUNKING
# ==============================================================================
chunk_content_approx <- function(content, chunk_chars = 12000) {
    n <- nchar(content)
    if (n <= chunk_chars) {
        return(content)
    }
    starts <- seq(1, n, by = chunk_chars)
    ends   <- pmin(starts + chunk_chars - 1, n)
    mapply(substr, content, starts, ends, USE.NAMES = FALSE)
}

insert_vectors <- function(
        con,
        df,
        embed_fun     = embed_openai(),
        chunk_chars   = 12000,
        embedding_dim = 1536
) {
    if (!"page_content" %in% names(df)) {
        if ("content" %in% names(df)) {
            df$page_content <- df$content
        } else {
            stop("Data frame must have 'page_content' or 'content' column.")
        }
    }

    expanded <- list()
    idx <- 1
    for (i in seq_len(nrow(df))) {
        row_i <- df[i, ]
        splitted <- chunk_content_approx(row_i$page_content, chunk_chars)
        for (chunked_content in splitted) {
            row_cp <- row_i
            row_cp$page_content <- chunked_content
            expanded[[idx]] <- row_cp
            idx <- idx + 1
        }
    }
    df_expanded <- dplyr::bind_rows(expanded)
    if (!nrow(df_expanded)) return(invisible(NULL))

    # Embed if needed
    if (!"embedding" %in% names(df_expanded)) {
        emb_mat <- embed_fun(df_expanded$page_content)
        if (ncol(emb_mat) != embedding_dim) {
            stop(sprintf(
                "Embeddings dimension is %d, but embedding_dim=%d. Adjust code or embed_fun config.",
                ncol(emb_mat), embedding_dim
            ))
        }
        storage.mode(emb_mat) <- "double"
        df_expanded$embedding <- emb_mat
    } else {
        # Make sure all embeddings match dimension
        if (is.matrix(df_expanded$embedding)) {
            if (ncol(df_expanded$embedding) != embedding_dim) {
                stop(sprintf(
                    "Embeddings dimension is %d, but embedding_dim=%d. Mismatch.",
                    ncol(df_expanded$embedding), embedding_dim
                ))
            }
            storage.mode(df_expanded$embedding) <- "double"
        } else {
            df_expanded$embedding <- lapply(df_expanded$embedding, function(vec) {
                if (length(vec) != embedding_dim) {
                    stop(sprintf(
                        "One row has embedding dimension %d, but embedding_dim=%d. Mismatch.",
                        length(vec), embedding_dim
                    ))
                }
                storage.mode(vec) <- "double"
                vec
            })
        }
    }

    # Insert data
    n <- nrow(df_expanded)
    rows_sql <- character(n)
    for (i in seq_len(n)) {
        content_esc <- DBI::dbQuoteString(con, df_expanded$page_content[i])
        if (is.matrix(df_expanded$embedding)) {
            e_vec <- df_expanded$embedding[i, ]
        } else {
            e_vec <- df_expanded$embedding[[i]]
        }
        e_str  <- paste(e_vec, collapse = ",")
        e_expr <- sprintf("CAST(array_value(%s) AS FLOAT[])", e_str)
        rows_sql[i] <- sprintf("(%s,%s)", content_esc, e_expr)
    }

    insert_sql <- sprintf("
        INSERT INTO vectors(page_content, embedding)
        VALUES %s
    ", paste(rows_sql, collapse = ",\n"))

    DBI::dbExecute(con, insert_sql)
    invisible(NULL)
}

# ==============================================================================
#  7) BUILD INDEX & SEARCH
# ==============================================================================
build_vector_index <- function(
        store,
        type = c("vss", "fts")
) {
    con <- if (inherits(store, "DBIConnection")) store else store
    type <- match.arg(type, several.ok = TRUE)

    if ("vss" %in% type) {
        DBI::dbExecute(con, "SET hnsw_enable_experimental_persistence = true;")
        DBI::dbExecute(con, "DROP INDEX IF EXISTS idx_vectors_embedding;")
        DBI::dbExecute(con, "
            CREATE INDEX idx_vectors_embedding
            ON vectors
            USING HNSW(embedding);
        ")
    }

    if ("fts" %in% type) {
        DBI::dbExecute(con, "INSTALL fts; LOAD fts;")
        DBI::dbExecute(con, "
            PRAGMA create_fts_index(
              'vectors',
              'id',
              'page_content',
              overwrite = 1
            );
        ")
    }

    invisible(store)
}

search_vectors <- function(
        con,
        query_text,
        top_k         = 5,
        embed_fun     = embed_openai(),
        embedding_dim = 1536
) {
    q_emb <- embed_fun(query_text)
    if (ncol(q_emb) != embedding_dim) {
        stop(sprintf(
            "Query embedding dimension is %d, but embedding_dim=%d. Mismatch.",
            ncol(q_emb), embedding_dim
        ))
    }
    storage.mode(q_emb) <- "double"

    # Create a temp table for the query vector
    DBI::dbExecute(con, "DROP TABLE IF EXISTS __temp_query__;")
    create_tmp_sql <- sprintf("
        CREATE TEMP TABLE __temp_query__ (
          embedding FLOAT[%d]
        );
    ", embedding_dim)
    DBI::dbExecute(con, create_tmp_sql)

    emb_str  <- paste(q_emb[1, ], collapse = ",")
    arr_expr <- sprintf("array_value(%s)", emb_str)
    insert_tmp_sql <- sprintf("
        INSERT INTO __temp_query__(embedding)
        VALUES (%s)
    ", arr_expr)
    DBI::dbExecute(con, insert_tmp_sql)

    # HNSW distance operator is <=> in DuckDB. Lower is more similar.
    sql <- sprintf("
        SELECT v.id, v.page_content, v.embedding <=> (SELECT embedding FROM __temp_query__) AS dist
        FROM vectors v
        ORDER BY dist ASC
        LIMIT %d;
    ", top_k)

    res <- DBI::dbGetQuery(con, sql)
    DBI::dbExecute(con, "DROP TABLE IF EXISTS __temp_query__;")
    res
}

# ==============================================================================
#  8) RAG CHAIN IMPLEMENTATION
# ==============================================================================

#' Create a Retrieval-Augmented Generation (RAG) Chain
#'
#' Creates a LangChain-style RAG chain using DuckDB for vector store operations, optional Tavily API for web search,
#' and in-memory message history for conversational context.
#'
#' @param llm A function that takes a prompt and returns a response (e.g. a call to OpenAI or Claude).
#' @param vector_database_directory Path to DuckDB database file.
#' @param method Currently only "DuckDB" is supported.
#' @param embedding_function A function for embedding text. Defaults to `embed_openai()`.
#' @param system_prompt Optional prompt with placeholders \code{{chat_history}}, \code{{input}}, \code{{context}}
#' @param chat_history_prompt Prompt used to rephrase user questions based on prior context.
#' @param tavily_search API key for Tavily (or NULL to disable web search).
#' @param embedding_dim Dimensionality of embedding vectors (default 1536).
#' @param use_web_search Logical, whether to include web results from Tavily (default TRUE).
#'
#' @return A list of utility functions:
#' \itemize{
#'   \item \code{invoke(text)} — Performs full context retrieval + LLM response
#'   \item \code{custom_invoke(text)} — Retrieves context only, no LLM response
#'   \item \code{get_session_history()} — Returns full chat history
#'   \item \code{clear_history()} — Clears the chat memory
#'   \item \code{disconnect()} — Closes DuckDB connection
#' }
#'
#' @export

create_rag_chain <- function(
        llm,
        vector_database_directory,
        method             = "DuckDB",
        embedding_function = NULL,
        system_prompt      = NULL,
        chat_history_prompt = NULL,
        tavily_search      = NULL,
        embedding_dim      = 1536,
        use_web_search     = TRUE
) {
    # Validate method
    if (method != "DuckDB") {
        stop("Only 'DuckDB' method is supported in this implementation. Please choose 'DuckDB'.")
    }

    # Default embedding function
    if (is.null(embedding_function)) {
        embedding_function <- embed_openai(model = "text-embedding-ada-002")
    }

    # Connect to DuckDB
    con <- connect_vectorstore(db_path = vector_database_directory, read_only = FALSE)

    # Build or rebuild the vector index
    build_vector_index(con, type = c("vss", "fts"))

    # Default prompts
    if (is.null(system_prompt)) {
        system_prompt <- default_system_prompt
    }
    if (is.null(chat_history_prompt)) {
        chat_history_prompt <- contextualize_q_system_prompt
    }

    # Initialize message history
    message_history <- SimpleMessageHistory()

    # Helper to reformulate the question in isolation
    contextualize_question <- function(input_text, chat_history, ignore_history = FALSE) {
        if (ignore_history || length(chat_history) == 0) {
            return(input_text)
        }
        # Format chat history as a string
        history_text <- paste(sapply(chat_history, function(msg) {
            paste(msg$role, ":", msg$content)
        }), collapse = "\n")

        # Fill the prompt
        prompt <- format_prompt(
            chat_history_prompt,
            list(
                chat_history = history_text,
                input        = input_text
            )
        )

        # This LLM call is only to reformulate the question, not to answer
        reformulated_question <- llm(prompt)
        reformulated_question
    }

    # The final step: gather context and produce an answer
    answer_question <- function(input_text, chat_history, documents) {
        # Combine the retrieved documents
        context <- if (is.null(documents)) {
            "No relevant context found."
        } else {
            paste(sapply(documents, `[[`, "page_content"), collapse = "\n\n")
        }

        # Format the previous chat as a string (optional usage below)
        history_text <- if (length(chat_history) == 0) {
            "No previous messages."
        } else {
            paste(sapply(chat_history, function(msg) {
                paste(msg$role, ":", msg$content)
            }), collapse = "\n")
        }

        # Use our system prompt, ensuring placeholders for {input}, {context}, {chat_history}
        prompt <- format_prompt(
            system_prompt,
            list(
                chat_history = history_text,
                input        = input_text,
                context      = context
            )
        )

        # Get the final answer
        answer <- llm(prompt)
        answer
    }

    # Custom function that just retrieves context without generating the final answer
    custom_invoke <- function(input_text) {
        chat_history <- message_history$get_messages()
        standalone_question <- contextualize_question(input_text, chat_history)
        documents <- perform_web_search(
            input_text    = standalone_question,
            con           = con,
            embed_fun     = embedding_function,
            embedding_dim = embedding_dim,
            tavily_search = tavily_search,
            use_web_search = use_web_search
        )
        list(
            chat_history = chat_history,
            input        = input_text,
            documents    = documents
        )
    }

    # Main user-facing function: returns the answer plus updated chat history
    invoke <- function(input_text, ignore_history = FALSE) {
        chat_history <- message_history$get_messages()
        standalone_question <- contextualize_question(input_text, chat_history, ignore_history)

        documents <- perform_web_search(
            input_text    = standalone_question,
            con           = con,
            embed_fun     = embedding_function,
            embedding_dim = embedding_dim,
            tavily_search = tavily_search,
            use_web_search = use_web_search
        )

        answer <- answer_question(standalone_question, chat_history, documents)

        # Update in-memory chat history
        message_history$add_messages(list(
            list(role = "human",    content = input_text),
            list(role = "assistant", content = answer)
        ))

        list(
            input        = input_text,
            chat_history = message_history$get_messages(),
            documents    = documents,
            answer       = answer
        )
    }

    # Return the chain object
    list(
        invoke             = invoke,
        custom_invoke      = custom_invoke,
        get_session_history= function() message_history$get_messages(),
        clear_history      = function() message_history$clear_messages(),
        disconnect         = function() DBI::dbDisconnect(con)
    )
}

