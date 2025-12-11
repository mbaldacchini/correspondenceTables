#' @title Retrieve correspondence tables between statistical classifications from CELLAR and FAO repositories.
#'
#' @description
#' This function retrieves a correspondence (mapping) table between two
#' statistical classifications stored as Linked Open Data in the CELLAR or
#' FAO repositories.
#'
#' @param endpoint Character string indicating which SPARQL endpoint to
#'   query. Must be one of:
#'   \itemize{
#'     \item \code{"CELLAR"}
#'     \item \code{"FAO"}
#'   }
#'
#' @param prefix Character string giving the namespace prefix of the
#'   correspondence table itself.
#'   This prefix must match the prefix used to identify the correspondence
#'   scheme in the CELLAR or FAO repository (e.g. \code{"cn2022"},
#'   \code{"cpa2015"}, etc.).
#'
#' @param ID_table Character string giving the identifier of the
#'   correspondence table to retrieve (e.g. \code{"CN2022_NST2007"}).
#'
#' @param language Character string indicating the language used for labels
#'   and notes. Defaults to \code{"en"}.
#'
#' @param CSVout Logical or character. Controls CSV export:
#'   \itemize{
#'     \item \code{FALSE} (default): no file is written,
#'     \item \code{TRUE}: the table is written to \code{<ID_table>_table.csv},
#'     \item character: explicit filepath where the table is saved.
#'   }
#'
#' @param showQuery Logical. If \code{TRUE} (default), the function returns
#'   a list containing both the SPARQL query and the resulting correspondence
#'   table. If \code{FALSE}, only the table (data frame) is returned.
#'
#' @return
#' If \code{showQuery = TRUE}, a list with:
#' \itemize{
#'   \item \code{SPARQL.query}: the full SPARQL query,
#'   \item \code{CorrespondenceTable}: a data frame with one row per mapping.
#' }
#'
#' If \code{showQuery = FALSE}, only the correspondence table (data frame)
#' is returned.
#'
#' @seealso
#' \code{\link{classificationEndpoint}},
#' \code{\link{retrieveClassificationTable}}
#'
#' @examples
#' \dontrun{
#'   endpoint <- "CELLAR"
#'   prefix   <- "cn2022"
#'   ID_table <- "CN2022_NACE2"
#'
#'   res <- retrieveCorrespondenceTable(
#'     endpoint = endpoint,
#'     prefix   = prefix,
#'     ID_table = ID_table
#'   )
#'
#'   cat(res$SPARQL.query)
#'   head(res$CorrespondenceTable)
#' }
#'
#' @import httr
#' @export
retrieveCorrespondenceTable <- function(endpoint,
                                        prefix,
                                        ID_table,
                                        language = "en",
                                        CSVout = FALSE,
                                        showQuery = TRUE) {
  # ton code ici...
}



retrieveCorrespondenceTable = function(endpoint,
                                       prefix,
                                       ID_table,
                                       language = "en",
                                       CSVout   = FALSE,
                                       showQuery = TRUE) {
  #-------------------------------
  # 0. Normalize / validate endpoint
  #-------------------------------
  endpoint <- toupper(endpoint)
  if (!endpoint %in% c("CELLAR", "FAO")) {
    stop(simpleError(
      paste("The endpoint value:", endpoint, "is not accepted. Use 'CELLAR' or 'FAO'.")
    ))
  }
  
  #-------------------------------
  # 1. Define SPARQL endpoint URL
  #-------------------------------
  if (endpoint == "CELLAR") {
    source <- "http://publications.europa.eu/webapi/rdf/sparql"
  }
  if (endpoint == "FAO") {
    source <- "https://stats.fao.org/caliper/sparql/AllVocs"
  }
  
  #---------------------------------------------------------
  # 2. QTM requirement: pre-check via correspondenceTableList
  #---------------------------------------------------------
  ct_list <- tryCatch(
    correspondenceTableList(endpoint),
    error = function(e) {
      stop(simpleError(
        paste0(
          "correspondenceTableList() failed when called from retrieveCorrespondenceTable(",
          "endpoint = '", endpoint, "', prefix = '", prefix,
          "', ID_table = '", ID_table, "', language = '", language, "').\n",
          "Original error from correspondenceTableList(): ",
          conditionMessage(e),
          "\nNo SPARQL request was executed."
        )
      ))
    }
  )
  
  # correspondenceTableList(endpoint) retourne une liste de data.frames par préfixe
  if (is.data.frame(ct_list)) {
    ct_df <- ct_list
  } else if (is.list(ct_list) && all(vapply(ct_list, is.data.frame, logical(1)))) {
    if (length(ct_list) == 0L) {
      stop(simpleError(
        paste0(
          "correspondenceTableList('", endpoint, "') returned an empty list.\n",
          "No SPARQL request was executed from retrieveCorrespondenceTable()."
        )
      ))
    }
    ct_df <- do.call(rbind, ct_list)
    rownames(ct_df) <- NULL
  } else {
    stop(simpleError(
      paste0(
        "Unexpected object returned by correspondenceTableList('", endpoint,
        "') when called from retrieveCorrespondenceTable().\n",
        "No SPARQL request was executed."
      )
    ))
  }
  
  # On s'attend à avoir au moins les colonnes: prefix, ID_table
  if (!all(c("prefix", "ID_table") %in% names(ct_df))) {
    stop(simpleError(
      paste0(
        "Result of correspondenceTableList('", endpoint,
        "') does not contain expected columns 'prefix' and 'ID_table'.\n",
        "No SPARQL request was executed from retrieveCorrespondenceTable()."
      )
    ))
  }
  
  # CorrespondenceTableList stocke le prefix en majuscule, on fait une comparaison insensible à la casse
  match_row <- which(
    tolower(ct_df$prefix) == tolower(prefix) &
      ct_df$ID_table == ID_table
  )
  
  if (length(match_row) == 0L) {
    # (2b) non satisfait ⇒ pas de SPARQL
    stop(simpleError(
      paste0(
        "The combination (prefix = '", prefix, "', ID_table = '", ID_table,
        "') is not available for endpoint '", endpoint,
        "' according to correspondenceTableList().\n",
        "No SPARQL request was executed."
      )
    ))
  }
  # Si on arrive ici, la combinaison (prefix, ID_table) est bien disponible :
  # → on peut exécuter la requête SPARQL comme avant.
  
  #-------------------------------
  # 3. Déduire A et B depuis ID_table
  #-------------------------------
  ID_table_temp <- gsub("-", "_", ID_table)
  ID_table_temp <- gsub("__", "_", ID_table_temp)
  A <- sub("_.*", "", ID_table_temp)
  B <- sub(".*_", "", ID_table_temp)
  
  #-------------------------------
  # 4. Prefixes SPARQL
  #-------------------------------
  prefixlist <- prefixList(endpoint, prefix = tolower(c(A, B)))
  prefixlist <- as.character(paste(prefixlist, collapse = "\n"))
  
  #-------------------------------
  # 5. Construction de la requête SPARQL
  #-------------------------------
  SPARQL.query_0 <- paste0(
    prefixlist, "
    SELECT ?", A, " ?", B, " ?Label_", A, " ?Label_", B,
    " ?Include_", A, " ?Exclude_", A,
    " ?Include_", B, " ?Exclude_", B,
    " ?Comment ?URL  ?Sourcedatatype ?Targetdatatype 

    WHERE {
      ", prefix, ":", ID_table, " xkos:madeOf ?Associations .
      ?Associations xkos:sourceConcept ?Source .
      OPTIONAL  {?Associations xkos:targetConcept ?Target .}
      OPTIONAL  {?Associations rdfs:comment ?Comment . }  

      ?Source   skos:notation ?SourceNotation .
      ?Target   skos:notation ?TargetNotation .

      BIND (STR(?Associations)   AS ?URL)
      BIND (STR(?SourceNotation) AS ?", A, ")
      BIND (STR(?TargetNotation) AS ?", B, ")
      BIND (datatype(?SourceNotation) AS ?Sourcedatatype)
      BIND (datatype(?TargetNotation) AS ?Targetdatatype)

      OPTIONAL { ?Source skos:altLabel ?Label_", A,
    "  FILTER (LANG(?Label_", A, ") = '", language, "') .}
      OPTIONAL { ?Target skos:altLabel ?Label_", B,
    "  FILTER (LANG(?Label_", B, ") = '", language, "') .}
      OPTIONAL { ?Source skos:scopeNote ?Include_", A,
    ".  FILTER (LANG(?Include_", A, ") = '", language, "') .}
      OPTIONAL { ?Source xkos:exclusionNote ?Exclude_", A,
    ". FILTER (LANG(?Exclude_", A, ") = '", language, "').}
      OPTIONAL { ?Target skos:scopeNote ?Include_", B,
    ".  FILTER (LANG(?Include_", B, ") = '", language, "') .}
      OPTIONAL { ?Target xkos:exclusionNote ?Exclude_", B,
    ". FILTER (LANG(?Exclude_", B, ") = '", language, "').}
   "
  )
  
  SPARQL.query_end <- paste0(
    "}
      ORDER BY ?Source
    "
  )
  
  SPARQL.query <- paste0(SPARQL.query_0, SPARQL.query_end)
  
  #-------------------------------
  # 6. Executing the SPARQL query
  #-------------------------------
  response <- httr::POST(
    url   = source,
    httr::accept("text/csv"),
    body  = list(query = SPARQL.query),
    encode = "form"
  )
  
  data <- data.frame(content(response, show_col_types = FALSE))
  
  #-------------------------------
  # 7. Post-processing: data types, cleaning, CSV, return
  #-------------------------------
  
  # Garder seulement les plainLiteral si plusieurs datatypes
  Source_type <- unique(data$Sourcedatatype)
  Target_type <- unique(data$Targetdatatype)
  # if (length(Source_type) > 1L || length(Target_type) > 1L) {
  #   data <- data[data$Sourcedatatype ==
  #                  "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral", ]
  #   data <- data[data$Targetdatatype ==
  #                  "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral", ]
  # }
  
  # Retirer les colonnes de datatype (les deux dernières)
  if (ncol(data) >= 2L) {
    data <- data[, 1:(ncol(data) - 2L), drop = FALSE]
  }
  
  # Nettoyer les sauts de ligne
  data <- lapply(data, function(x) gsub("\n", " ", x))
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  # Export CSV si demandé
  if (identical(CSVout, TRUE)) {
    name_csv <- paste0(ID_table, "_table.csv")
    utils::write.csv(data, file = name_csv, row.names = FALSE)
    message(paste0("The correspondence table was saved in ", getwd(), "/", name_csv))
  } else if (is.character(CSVout)) {
    utils::write.csv(data, file = CSVout, row.names = FALSE)
    message(paste0("The table was saved in ", CSVout))
  }
  
  # Structure de sortie
  if (isTRUE(showQuery)) {
    result <- list(
      SPARQL.query        = SPARQL.query,
      CorrespondenceTable = data
    )
    cat(result$SPARQL.query, sep = "\n")
  } else {
    result <- data
  }
  
  return(result)
}
