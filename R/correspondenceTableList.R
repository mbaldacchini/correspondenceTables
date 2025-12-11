#' @title Retrieve a list of all correspondence tables in the CELLAR and FAO repositories
#'
#' @description
#' List all correspondence tables in the CELLAR or FAO repositories.
#'
#' @param endpoint Character. SPARQL endpoint(s) to query. Valid values are:
#'   \code{"CELLAR"}, \code{"FAO"}, or \code{"ALL"} (default).
#'
#' @param showQuery Logical; if \code{TRUE}, the SPARQL query used for each
#'   prefix is printed (and can be inspected by the user). This is mainly
#'   intended for debugging or reproducibility. Default is \code{FALSE}.
#'
#' @return
#' If \code{endpoint = "CELLAR"} or \code{"FAO"}, returns a \code{data.frame} with:
#' \itemize{
#'   \item Prefix
#'   \item ID
#'   \item Source.Classification
#'   \item Target.Classification
#'   \item Table.Name
#'   \item URI
#' }
#' If \code{endpoint = "ALL"}, returns a named list of two \code{data.frame}s:
#' one for each endpoint.
#'
#' @details
#' The behaviour of this function is contingent on the global option
#' \code{useLocalDataForVignettes}:
#' the default behaviour (when the option is not set, or set to something else
#' than \code{TRUE}), is that it queries live SPARQL endpoints online.
#' When the option is set to \code{TRUE} via
#' \code{options(useLocalDataForVignettes = TRUE)}, it may return local
#' (embedded) data instead of querying live SPARQL endpoints.
#' This is useful for building vignettes or offline testing.
#'
#' @import httr
#' @import jsonlite
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Retrieve list of all available correspondence tables
#'   corr_list <- correspondenceTableList("ALL")
#'   print(corr_list)
#' }
correspondenceTableList <- function(endpoint = "ALL", showQuery = FALSE) {
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  
  # ---- Offline / vignette mode ----
  if (getOption("useLocalDataForVignettes", FALSE)) {
    if (endpoint == "ALL") {
      return(list(
        CELLAR = correspondenceTableList("CELLAR", showQuery = showQuery),
        FAO    = correspondenceTableList("FAO",    showQuery = showQuery)
      ))
    }
    path <- system.file(
      "extdata",
      paste0("correspondenceTableList_", endpoint, ".csv"),
      package = "correspondenceTables"
    )
    if (!file.exists(path)) {
      stop("Local file for correspondenceTableList and endpoint '", endpoint,
           "' is missing. Expected at: ", path)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE)
    return(df)
  }
  
  # ---- If ALL in online mode: recurse on both endpoints ----
  if (endpoint == "ALL") {
    return(list(
      CELLAR = correspondenceTableList("CELLAR", showQuery = showQuery),
      FAO    = correspondenceTableList("FAO",    showQuery = showQuery)
    ))
  }
  
  # ---- Online mode for a single endpoint ----
  config <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/eurostat/correspondenceTables/refs/heads/main/inst/extdata/endpoint_source_config.json"
  )
  source <- config[[endpoint]]
  sep <- ifelse(endpoint == "CELLAR", "_", "-")
  rm <- 1:16
  
  prefixlist_raw <- prefixList(endpoint)
  prefixlist_raw <- gsub(
    "PREFIX FPC&D2022: <https://stats.fao.org/classifications/FPC&D2022/>",
    "",
    prefixlist_raw
  )
  prefix_header <- as.character(paste(prefixlist_raw, collapse = "\n"))
  
  prefixes_loop <- unlist(lapply(strsplit(as.character(prefixList(endpoint)), " "), function(x) x[2]))
  prefixes_loop <- prefixes_loop[-rm]
  
  data_t <- list()
  
  for (i in seq_along(prefixes_loop)) {
    prefix <- prefixes_loop[i]
    
    # Exceptions where the separator changes
    if (prefix %in% c("FCL:", "ICC10:", "ICC11:", "ISIC4:")) {
      sep <- "--"
    }
    
    tryCatch({
      SPARQL.query <- paste0(
        prefix_header, "
        SELECT ?ID_table ?A ?B ?Table ?URL 
        WHERE {
          ?s a xkos:Correspondence ;
             skos:prefLabel ?Label .
          BIND (STR(?s) AS ?URL)
          BIND (STR(?Label) AS ?Table)
          BIND (STRAFTER(STR(?s), STR(", prefix, ")) AS ?ID_table)
          BIND (STRAFTER(STR(?ID_table), '", sep, "') AS ?B)
          BIND (STRBEFORE(STR(?ID_table), '", sep, "') AS ?A)
          FILTER (STRLEN(?ID_table) != 0)
        }
      "
      )
      
      if (isTRUE(showQuery)) {
        message("SPARQL query for endpoint = ", endpoint,
                ", prefix = ", prefix, ":\n", SPARQL.query)
      }
      
      response <- httr::POST(
        url   = source,
        httr::accept("text/csv"),
        body  = list(query = SPARQL.query),
        encode = "form"
      )
      df <- utils::read.csv(
        text = httr::content(response, "text"),
        stringsAsFactors = FALSE
      )
      
      if (nrow(df) == 0) {
        df <- data.frame(prefix = character(), df)
      } else {
        df <- cbind(prefix = rep(gsub(":", "", prefix), nrow(df)), df)
      }
      
      data_t[[i]] <- df
      names(data_t)[i] <- prefix
      
    }, error = function(e) {
      message("The following SPARQL code was used:\n", SPARQL.query)
      stop(simpleError(
        paste0(
          "Error in function correspondenceTableList(", endpoint,
          "), endpoint not available or returned unexpected data."
        )
      ))
    })
  }
  
  # ---- Bind all prefixes into a single data.frame ----
  if (length(data_t) == 0L) {
    out <- data.frame(
      Prefix               = character(),
      ID                   = character(),
      Source.Classification = character(),
      Target.Classification = character(),
      Table.Name           = character(),
      URI                  = character(),
      stringsAsFactors     = FALSE
    )
  } else {
    out <- do.call(rbind, data_t)
    # Expected columns: prefix, ID_table, A, B, Table, URL
    if (ncol(out) >= 6) {
      colnames(out)[1:6] <- c(
        "Prefix",
        "ID",
        "Source.Classification",
        "Target.Classification",
        "Table.Name",
        "URI"
      )
    }
  }
  
  out
}
