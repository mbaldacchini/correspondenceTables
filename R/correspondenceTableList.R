#' @title Retrieve correspondence tables between statistical classifications from CELLAR and FAO repositories.
#' 
#' @description
#' Retrieves a correspondence (mapping) table between two statistical classifications (SKOS/XKOS)
#' stored as Linked Open Data in the CELLAR or FAO repositories.
#' 
#' @param endpoint Character: "CELLAR" or "FAO".
#' @param prefix Character: namespace prefix of the correspondence table (e.g., "cn2022").
#' @param ID_table Character: identifier of the correspondence (e.g., "CN2022_NACE2").
#' @param language Character: label language (default "en").
#' @param CSVout Logical or character: FALSE (default), TRUE (auto filename), or explicit path.
#' @param showQuery Logical: TRUE returns list(query, table); FALSE returns data.frame only.
#' 

#' @return
#' If \code{showQuery = FALSE}, returns a \code{data.frame} where each row
#' represents one mapping between a source concept and a target concept.
#'
#' The columns returned are:
#'
#' \itemize{
#'
#'   \item \code{<A>}  
#'         The notation/code of the source concept (e.g. "CN2022" code).
#'
#'   \item \code{<B>}  
#'         The notation/code of the target concept (e.g. "NACE2" code).  
#'
#'   \item \code{Label_<A>}  
#'         Human‑readable label of the source concept in the selected language
#'         (prefLabel or altLabel via COALESCE).
#'
#'   \item \code{Label_<B>}  
#'         Human‑readable label of the target concept in the selected language
#'         (prefLabel or altLabel).  
#'
#'   \item \code{Include_<A>}  
#'         SKOS \code{scopeNote} attached to the source concept.  
#'         Represents textual information describing what is *included* in the definition
#'         of the source concept.  
#'         Often empty; presence depends on the classification metadata.
#'
#'   \item \code{Exclude_<A>}  
#'         XKOS \code{exclusionNote} on the source concept.  
#'         Describes what is *explicitly excluded* from the meaning of the concept.
#'
#'   \item \code{Include_<B>}  
#'         SKOS \code{scopeNote} for the target concept.  
#'         Same meaning as \code{Include_<A>} but on the target side.
#'
#'   \item \code{Exclude_<B>}  
#'         XKOS \code{exclusionNote} for the target concept.  
#'         Same semantics as \code{Exclude_<A>}, but for the target.
#'
#'   \item \code{Comment}  
#'         Free‑text comment attached to the correspondence association
#'         (from \code{rdfs:comment}).  
#'
#'   \item \code{URL}  
#'         String representation of the association (the internal URI of the mapping link).
#' }
#'
#' If \code{showQuery = TRUE}, returns a list with two elements:
#' \itemize{
#'   \item \code{SPARQL.query}: the full query issued to the endpoint;
#'   \item \code{CorrespondenceTable}: the data.frame described above.
#' }
#' 
#' @import httr
#' 
#' @export

retrieveCorrespondenceTable <- function(endpoint,
                                        prefix,
                                        ID_table,
                                        language = "en",
                                        CSVout   = FALSE,
                                        showQuery = FALSE) {
  #-------------------------------
  # 0) Normalize / validate endpoint
  #-------------------------------
  endpoint <- toupper(trimws(endpoint))
  if (!endpoint %in% c("CELLAR", "FAO")) {
    stop(simpleError(
      paste("The endpoint value:", endpoint, "is not accepted. Use 'CELLAR' or 'FAO'.")
    ))
  }
  
  #-------------------------------
  # 1) SPARQL endpoint URL
  #-------------------------------
  endpoint_url <- if (endpoint == "CELLAR") {
    "http://publications.europa.eu/webapi/rdf/sparql"
  } else {
    # "https://stats.fao.org/caliper/sparql/AllVocs"
    "https://caliper.integratedmodelling.org/caliper/sparql"  
  }
  
  #---------------------------------------------------------
  # 2) Pre-check via correspondenceTableList()
  #---------------------------------------------------------
  ct_list <- tryCatch(
    correspondenceTableList(endpoint),
    error = function(e) {
      stop(simpleError(
        paste0(
          "correspondenceTableList() failed when called from retrieveCorrespondenceTable(",
          "endpoint = '", endpoint, "', prefix = '", prefix,
          "', ID_table = '", ID_table, "', language = '", language, "').\n",
          "Original error: ", conditionMessage(e),
          "\nNo SPARQL request was executed."
        )
      ))
    }
  )
  
  # Normalize ct_list -> ct_df (supports data.frame, list<df>, or matrix)
  if (is.data.frame(ct_list)) {
    ct_df <- ct_list
    
  } else if (is.list(ct_list) && length(ct_list) > 0L &&
             all(vapply(ct_list, is.data.frame, logical(1)))) {
    ct_df <- do.call(rbind, ct_list)
    rownames(ct_df) <- NULL
    
  } else if (is.matrix(ct_list)) {
    # Matrix with rownames as field names and columns as entries (observed for CELLAR)
    m  <- ct_list
    df <- as.data.frame(t(m), stringsAsFactors = FALSE)
    colnames(df) <- rownames(m)
    rownames(df) <- NULL
    ct_df <- df
    
  } else {
    stop(simpleError(
      paste0(
        "Unexpected object returned by correspondenceTableList('", endpoint,
        "') when called from retrieveCorrespondenceTable().\n",
        "No SPARQL request was executed."
      )
    ))
  }
  
  #-------------------------------
  # 2b) Standardize schema & validate
  #-------------------------------
  names(ct_df) <- tolower(gsub("\\s+", ".", names(ct_df)))
  # Map variants
  if ("id" %in% names(ct_df) && !"id_table" %in% names(ct_df)) {
    ct_df$id_table <- ct_df$id
  }
  if ("table.name" %in% names(ct_df) && !"table_name" %in% names(ct_df)) {
    ct_df$table_name <- ct_df[["table.name"]]
  }
  # Validate
  required <- c("prefix", "id_table")
  missing  <- setdiff(required, names(ct_df))
  if (length(missing)) {
    stop(simpleError(
      paste0(
        "Result of correspondenceTableList('", endpoint,
        "') is missing columns: ", paste(missing, collapse = ", "), ".\n",
        "No SPARQL request was executed from retrieveCorrespondenceTable()."
      )
    ))
  }
  
  # Case-insensitive match on prefix + exact match on ID_table
  match_row <- which(
    tolower(ct_df$prefix) == tolower(prefix) &
      ct_df$id_table == ID_table
  )
  if (length(match_row) == 0L) {
    stop(simpleError(
      paste0(
        "The combination (prefix = '", prefix, "', ID_table = '", ID_table,
        "') is not available for endpoint '", endpoint,
        "' according to correspondenceTableList().\n",
        "No SPARQL request was executed."
      )
    ))
  }
  match_row <- match_row[1]
  
  # Authoritative URI, if available
  corr_uri <- NULL
  if ("uri" %in% names(ct_df)) {
    corr_uri <- ct_df$uri[match_row]
    if (!is.character(corr_uri) || is.na(corr_uri) || !nzchar(trimws(corr_uri))) {
      corr_uri <- NULL
    }
  }
  
  #-------------------------------
  # 3) Derive A and B from ID_table
  #-------------------------------
  ID_table_temp <- gsub("-", "_", ID_table)
  ID_table_temp <- gsub("__", "_", ID_table_temp, fixed = TRUE)
  A <- sub("_.*", "", ID_table_temp)
  B <- sub(".*_", "", ID_table_temp)
  
  #-------------------------------
  # 4) SPARQL prefixes
  #-------------------------------
  prefixlist <- prefixList(endpoint, prefix = tolower(c(A, B)))
  prefixlist <- as.character(paste(prefixlist, collapse = "\n"))
  
  #-------------------------------
  # 5) Build SPARQL query
  #   - Anchor to <URI> if known; otherwise use prefix:ID_table
  #   - Make Target-related triples fully OPTIONAL
  #   - Use COALESCE(prefLabel, altLabel) for better label coverage
  #-------------------------------
  anchor <- if (!is.null(corr_uri)) paste0("<", corr_uri, ">") else paste0(prefix, ":", ID_table)
  
  SPARQL.query_0 <- paste0(
    prefixlist, "\n",
    "SELECT ?", A, " ?", B, " ?Label_", A, " ?Label_", B,
    " ?Include_", A, " ?Exclude_", A,
    " ?Include_", B, " ?Exclude_", B,
    " ?Comment ?URL ?Sourcedatatype ?Targetdatatype\n",
    "WHERE {\n",
    "  ", anchor, " xkos:madeOf ?Associations .\n",
    "  ?Associations xkos:sourceConcept ?Source .\n",
    "  OPTIONAL { ?Associations xkos:targetConcept ?Target . }\n",
    "  OPTIONAL { ?Associations rdfs:comment ?Comment . }\n",
    "\n",
    "  # Source (mandatory)\n",
    "  ?Source skos:notation ?SourceNotation .\n",
    "  OPTIONAL { ?Source skos:prefLabel ?PrefA FILTER (LANG(?PrefA) = '", language, "') }\n",
    "  OPTIONAL { ?Source skos:altLabel  ?AltA  FILTER (LANG(?AltA)  = '", language, "') }\n",
    "  OPTIONAL { ?Source skos:scopeNote      ?Include_", A, " FILTER (LANG(?Include_", A, ") = '", language, "') }\n",
    "  OPTIONAL { ?Source xkos:exclusionNote  ?Exclude_", A, " FILTER (LANG(?Exclude_", A, ") = '", language, "') }\n",
    "  BIND (COALESCE(?AltA, ?PrefA) AS ?Label_", A, ")\n",
    "\n",
    "  # Target (fully optional)\n",
    "  OPTIONAL {\n",
    "    ?Target skos:notation ?TargetNotation .\n",
    "    OPTIONAL { ?Target skos:prefLabel ?PrefB FILTER (LANG(?PrefB) = '", language, "') }\n",
    "    OPTIONAL { ?Target skos:altLabel  ?AltB  FILTER (LANG(?AltB)  = '", language, "') }\n",
    "    OPTIONAL { ?Target skos:scopeNote     ?Include_", B, " FILTER (LANG(?Include_", B, ") = '", language, "') }\n",
    "    OPTIONAL { ?Target xkos:exclusionNote ?Exclude_", B, " FILTER (LANG(?Exclude_", B, ") = '", language, "') }\n",
    "    BIND (COALESCE(?AltB, ?PrefB) AS ?Label_", B, ")\n",
    "    BIND (STR(?TargetNotation) AS ?", B, ")\n",
    "    BIND (datatype(?TargetNotation) AS ?Targetdatatype)\n",
    "  }\n",
    "\n",
    "  # URL + source bindings\n",
    "  BIND (STR(?Associations)   AS ?URL)\n",
    "  BIND (STR(?SourceNotation) AS ?", A, ")\n",
    "  BIND (datatype(?SourceNotation) AS ?Sourcedatatype)\n"
  )
  
  SPARQL.query_end <- paste0(
    "}\n",
    "ORDER BY ?", A, "\n"
  )
  
  SPARQL.query <- paste0(SPARQL.query_0, SPARQL.query_end)
  
  #-------------------------------
  # 6) Execute SPARQL
  #-------------------------------
  response <- httr::POST(
    url    = endpoint_url,
    httr::accept("text/csv"),
    body   = list(query = SPARQL.query),
    encode = "form",
    httr::user_agent("correspondenceTables (R)")
  )
  httr::stop_for_status(response)
  
  csv_text <- httr::content(response, as = "text", encoding = "UTF-8")
  data <- if (nzchar(csv_text)) {
    utils::read.csv(textConnection(csv_text), stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
  
  #-------------------------------
  # 7) Post-processing & return
  #-------------------------------
  # Drop datatype columns if present as last two
  if (ncol(data) >= 2L &&
      all(tail(names(data), 2L) %in% c("Sourcedatatype", "Targetdatatype"))) {
    data <- data[, 1:(ncol(data) - 2L), drop = FALSE]
  }
  
  # Clean newlines in character columns
  data[] <- lapply(data, function(x) if (is.character(x)) gsub("\n", " ", x) else x)
  
  # CSV export
  if (identical(CSVout, TRUE)) {
    name_csv <- paste0(ID_table, "_table.csv")
    utils::write.csv(data, file = name_csv, row.names = FALSE)
    message("The correspondence table was saved in ", file.path(getwd(), name_csv))
  } else if (is.character(CSVout)) {
    utils::write.csv(data, file = CSVout, row.names = FALSE)
    message("The table was saved in ", CSVout)
  }
  
  # Output
  if (isTRUE(showQuery)) {
    result <- list(
      SPARQL.query        = SPARQL.query,
      CorrespondenceTable = data
    )
    cat(result$SPARQL.query, sep = "\n")
    return(result)
  } else {
    return(data)
  }
}




