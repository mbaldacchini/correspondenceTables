#' @title Retrieve a classification table from CELLAR or FAO
#'
#' @description
#' Retrieves a full classification table (codes, labels, hierarchical
#' relationships, and associated notes) from the CELLAR or FAO SPARQL
#' repositories.
#' The function supports optional filtering by hierarchical level and
#' automatic use of local embedded data for vignette building.
#'
#' @param endpoint Character string specifying which SPARQL endpoint to query.
#'   Must be one of:
#'   \itemize{
#'     \item \code{"CELLAR"}
#'     \item \code{"FAO"}
#'   }
#'
#' @param prefix Character string giving the namespace prefix associated
#'   with the targeted classification scheme.
#'   This value should match the \code{Prefix} column returned by
#'   \code{\link{classificationList}} and is required to build:
#'   \itemize{
#'     \item the \code{PREFIX} declarations (via \code{prefixList}),
#'     \item the fully-qualified scheme name (e.g. \code{prefix:conceptScheme}).
#'   }
#'
#' @param conceptScheme Character string identifying the classification
#'   scheme to retrieve, typically obtained from the
#'   \code{ConceptScheme} column of \code{\link{classificationList}}.
#'
#' @param language Character string giving the language used for labels and
#'   notes. Defaults to \code{"en"}.
#'
#' @param level Character string specifying which hierarchical level to
#'   retrieve.
#'   \itemize{
#'     \item \code{"ALL"} (default): retrieve every level.
#'     \item A numeric depth (e.g. \code{"1"}, \code{"2"}): retrieve only
#'           that level.
#'   }
#'   If the classification has no defined levels (as detected by
#'   \code{\link{dataStructure}}), the function automatically resets
#'   \code{level = "ALL"} with a message.
#'
#' @param CSVout Boolean or character. Controls CSV export:
#'   \itemize{
#'     \item \code{NULL} (default): no export.
#'     \item \code{TRUE}: export to a default filename based on
#'           \code{prefix} and \code{language}.
#'     \item character: explicit path to a CSV file.
#'   }
#'
#' @param showQuery Boolean. If \code{TRUE} (default), returns a list
#'   containing both the SPARQL query and the resulting table.
#'   If \code{FALSE}, only the table (data frame) is returned.
#'
#' @details
#' Behaviour depends on the global option \code{useLocalDataForVignettes}:
#' \itemize{
#'   \item If \code{TRUE}:
#'         the function searches for a local embedded CSV file (in
#'         \code{inst/extdata}) matching the requested \code{prefix} and
#'         \code{language}. If found, it is returned immediately and no
#'         SPARQL call is made.
#'
#'   \item If \code{FALSE}:
#'         the function:
#'         \itemize{
#'           \item builds SPARQL prefix declarations via \code{prefixList},
#'           \item calls \code{\link{dataStructure}} to determine level
#'                 availability and adjust \code{level} when necessary,
#'           \item constructs and submits a SPARQL query to the selected
#'                 endpoint,
#'           \item returns the resulting classification table, optionally
#'                 along with the query used.
#'         }
#' }
#'
#' Clear, user-friendly errors are raised if:
#' \itemize{
#'   \item the endpoint is unreachable,
#'   \item the classification does not provide the requested level,
#'   \item \code{dataStructure()} indicates missing or invalid structure information,
#'   \item the SPARQL response is malformed.
#' }
#'
#' @return
#' If \code{showQuery = TRUE} (default), returns a list with:
#' \itemize{
#'   \item \code{SPARQL.query}: the full SPARQL query as a character string,
#'   \item \code{ClassificationTable}: a data frame containing the retrieved
#'         classification table.
#' }
#'
#' If \code{showQuery = FALSE}, only the classification table is returned.
#'
#' @examples
#' \dontrun{
#'   endpoint <- "CELLAR"
#'   prefix <- "cn2022"
#'   conceptScheme <- "cn2022"
#'
#'   res <- tryCatch(
#'     retrieveClassificationTable(
#'       endpoint      = endpoint,
#'       prefix        = prefix,
#'       conceptScheme = conceptScheme,
#'       language      = "en",
#'       level         = "ALL"
#'     ),
#'     error = function(e) {
#'       message("SPARQL query failed: ", e$message)
#'       NULL
#'     }
#'   )
#'
#'   if (!is.null(res)) {
#'     cat(res$SPARQL.query)
#'     head(res$ClassificationTable)
#'   }
#' }
#'


retrieveClassificationTable = function(endpoint,
                                       prefix,
                                       conceptScheme,
                                       language = "en",
                                       level = "ALL",
                                       CSVout = NULL,
                                       showQuery = FALSE
                                       ){
  # Check correctness of endpoint argument
  endpoint <- toupper(endpoint)
  if (!endpoint %in% c("CELLAR", "FAO")) {
    stop("`endpoint` must be either 'CELLAR' or 'FAO'.")
  }
  
  #-----------------------------------------------------------
  # 1. Local data branch (for vignettes / offline use)
  #-----------------------------------------------------------
  if (getOption("useLocalDataForVignettes", FALSE)) {
    localDataPath <- system.file(
      "extdata",
      paste0(prefix, "_", language, ".csv"),
      package = "correspondenceTables"
    )
    
    if (file.exists(localDataPath)) {
      data <- read.csv(localDataPath, stringsAsFactors = FALSE)
      
      # Optional level filtering on local data
      if (level != "ALL") {
        data <- data[nchar(gsub("\\.", "", data[[prefix]])) == as.numeric(level), ]
      }
      
      if (showQuery) {
        print("Data loaded from local file.")
      }
      return(data)
    }
  } else {
    
    #-----------------------------------------------------------
    # 2. Endpoint configuration + prefixes
    #-----------------------------------------------------------
    tryCatch(
      {
        # Try to load endpoint configuration from GitHub
        config <- tryCatch(
          jsonlite::fromJSON(
            "https://raw.githubusercontent.com/eurostat/correspondenceTables/main/inst/extdata/endpoint_source_config.json"
          ),
          error = function(e) NULL
        )
        
        # Default hardcoded endpoints
        default_endpoints <- list(
          CELLAR = "http://publications.europa.eu/webapi/rdf/sparql",
          FAO    = "https://stats.fao.org/caliper/sparql/AllVocs"
        )
        
        # Select source endpoint
        if (!is.null(config) && !is.null(config[[endpoint]])) {
          source <- config[[endpoint]]
        } else {
          source <- default_endpoints[[endpoint]]
        }
        
        # URL base (for information only)
        if (endpoint == "CELLAR") url <- "data.europa.eu/"
        if (endpoint == "FAO")    url <- "unstats.un.org/"
        
        # Load prefixes using prefixList()
        prefixlist <- prefixList(endpoint, prefix = prefix)
        prefixlist <- as.character(paste(prefixlist, collapse = "\n"))
        
      }, error = function(e) {
        stop(simpleError(
          paste(
            "Error in function retrieveClassificationTable:",
            "building of the SPARQL query failed",
            endpoint, "is not available or is returning unexpected data."
          )
        ))
      }
    )
    
    #-----------------------------------------------------------
    # 3. QTM requirement: run dataStructure() BEFORE SPARQL
    #-----------------------------------------------------------
    dt_level <- tryCatch(
      suppressMessages(
        dataStructure(endpoint, prefix, conceptScheme, language)
      ),
      error = function(e) {
        stop(simpleError(
          paste0(
            "Error in retrieveClassificationTable(): dataStructure() failed for ",
            "endpoint = '", endpoint, "', prefix = '", prefix,
            "', conceptScheme = '", conceptScheme, "', language = '", language, "'.\n",
            "Original error from dataStructure(): ", conditionMessage(e), "\n",
            "No SPARQL request was executed."
          )
        ))
      }
    )
    
    # ---- Case A: classification has NO levels ----
    if (nrow(dt_level) == 0L) {
      if (level != "ALL") {
        stop(simpleError(
          paste0(
            "Classification '", conceptScheme,
            "' has no level information, but a specific level ('", level,
            "') was requested.\n",
            "Level filtering cannot be applied. No SPARQL request was executed."
          )
        ))
      }
      # level == "ALL" we proceed without level filter
    }
    
    # ---- Case B: classification HAS levels & level != ALL ----
    if (nrow(dt_level) > 0L && level != "ALL") {
      # Depth is numeric in dt_level; level is character
      level_num <- suppressWarnings(as.numeric(level))
      
      if (is.na(level_num)) {
        stop(simpleError(
          paste0(
            "The requested level '", level,
            "' is not numeric. Only numeric levels (e.g. '1', '2', '3') are allowed.\n",
            "No SPARQL request was executed."
          )
        ))
      }
      
      available_levels <- unique(dt_level$Depth)
      
      if (!(level_num %in% available_levels)) {
        stop(simpleError(
          paste0(
            "Requested level '", level_num, "' does not exist for classification '",
            conceptScheme, "'.\nAvailable levels are: ",
            paste(sort(available_levels), collapse = ", "), ".\n",
            "No SPARQL request was executed."
          )
        ))
      }
    }
    
    #-----------------------------------------------------------
    # 4. Build and execute SPARQL query (only if all checks passed)
    #-----------------------------------------------------------
    tryCatch(
      {
        # Base query (all levels)
        SPARQL.query_0 <- paste0(
          prefixlist, "
        SELECT DISTINCT ?", prefix, " ?Name ?Level ?Parent ?Include ?Include_Also ?Exclude ?URL

        WHERE {
            ?s skos:altLabel ?Label ;
                skos:inScheme ?Scheme ;
                ^skos:member ?Member ;
                skos:notation ?notation .
            OPTIONAL {
              ?s skos:broader ?Broader .
              ?Broader skos:notation ?BT_Notation .
            }
            BIND (STR(?BT_Notation) as ?Parent)
            FILTER (?Scheme = ", prefix, ":", conceptScheme, ")
            FILTER (lang(?Label) = '", language, "')

            BIND (STR(?s)          AS ?URL)
            BIND (STR(?notation)   AS ?", prefix, " )
            BIND (STR(?Label)      AS ?Name)

            ?Member a xkos:ClassificationLevel ;
                    xkos:depth ?Depth ;
                    xkos:organizedBy ?L .
            ?L skos:prefLabel ?Level_Name .
            FILTER (LANG(?Level_Name)= '", language, "')
            BIND (STR(?Level_Name) AS ?LEVEL_S )
            BIND (STR(?Depth)      AS ?Level )

            OPTIONAL {?s skos:scopeNote ?Include .
                      FILTER (LANG(?Include) = '", language, "') .}
            OPTIONAL {?s xkos:exclusionNote ?Exclude .
                      FILTER (LANG(?Exclude) = '", language, "').}
            OPTIONAL {?s xkos:additionalContentNote ?Include_Also .
                      FILTER (LANG(?Include_Also) = '", language, "').}
        "
        )
        
        # Filter on Depth if a specific level was requested
        SPARQL.query_level <- paste0("FILTER (?Depth = ", level, ")")
        
        # Closing part
        SPARQL.query_end <- paste0(
          "}
          ORDER BY ?", prefix
        )
        
        if (length(level) == 0L) {
          stop("Classification level was not specified.")
        } else {
          if (level == "ALL") {
            SPARQL.query <- paste0(SPARQL.query_0, SPARQL.query_end)
          } else {
            SPARQL.query <- paste0(SPARQL.query_0, SPARQL.query_level, SPARQL.query_end)
          }
        }
        
        response <- httr::POST(
          url   = source,
          httr::accept("text/csv"),
          body  = list(query = SPARQL.query),
          encode = "form"
        )
        data <- data.frame(httr::content(response, show_col_types = FALSE))
        
      }, error = function(e) {
        cat("The following SPARQL code was used in the call:\n", SPARQL.query, "\n")
        message("The following response was given by the SPARQL call:")
        message(paste(capture.output(str(response)), collapse = "\n"))
        stop(simpleError(
          "Error in function retrieveClassificationTable, SPARQL query execution failed."
        ))
      }
    )
    
    #-----------------------------------------------------------
    # 5. Post-processing: datatype + duplicates + corrections + CSV + output
    #-----------------------------------------------------------
    
    # Keep only plainLiteral if multiple datatypes
    type <- unique(data$datatype)
    if (length(type) > 1L) {
      data <- data[data$datatype ==
                     "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral", ]
    }
    
    # Remove datatype column
    data <- data[, 1:(ncol(data) - 1), drop = FALSE]
    
    # Detect potential duplicate codes in the classification table
    code_col <- prefix
    key_cols <- c(code_col, "Name")
    key_cols <- key_cols[key_cols %in% names(data)]
    
    dup <- 0L
    if (length(key_cols) >= 1L) {
      dup <- sum(duplicated(data[key_cols]))
    }
    
    if (dup > 0L) {
      warning("There are duplicated codes in the classification table.")
    }
    
    # ---- NEW: apply correctionClassification() and warn if corrections applied ----
    corrected <- ClassificationCorrection(data, prefix)
    if (isTRUE(attr(corrected, "corrections_applied"))) {
      warning(
        "The raw SPARQL result contained inconsistencies that were corrected locally. ",
        "The returned table may differ from the raw endpoint result."
      )
    }
    data <- corrected
    # -------------------------------------------------------------------------------
    
    # Clean line breaks
    data <- lapply(data, function(x) gsub("\n", " ", x))
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    
    # Save results as CSV if requested
    CsvFileSave(CSVout, data)
    
    # Prepare result
    if (showQuery) {
      result <- list(
        SPARQL.query        = SPARQL.query,
        ClassificationTable = data
      )
      cat(result$SPARQL.query, sep = "\n")
    } else {
      result <- data
    }
    
    return(result)
  }
}
