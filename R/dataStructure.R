#' @title Retrieve the hierarchical structure of a classification scheme
#'
#' @description
#' This function retrieves structural information about a classification scheme
#' hosted in the CELLAR or FAO SPARQL repositories.  
#' It returns one row per hierarchical level and includes metadata such as:
#' \itemize{
#'   \item the name of the classification scheme,
#'   \item the depth of each level,
#'   \item the label of the level (in a selected language),
#'   \item the number of categories present at that level.
#' }
#'
#' @param endpoint Character string specifying which SPARQL endpoint to query.
#' Must be one of:
#' \itemize{
#'   \item \code{"CELLAR"},
#'   \item \code{"FAO"}.
#' }
#'
#' @param prefix Character string giving the namespace prefix used for the
#'   selected classification scheme in SPARQL.  
#'   This value must correspond to the \code{Prefix} column returned by
#'   \code{\link{classificationEndpoint}} for the chosen scheme, and it is
#'   used in the query to:
#'   \itemize{
#'     \item build the \code{PREFIX} declaration (\code{prefixList}),
#'     \item qualify the scheme and its members
#'           (e.g. \code{prefix:conceptScheme}, \code{prefix:SomeMember}).
#'   }
#'
#' @param conceptScheme The internal identifier of the classification scheme
#'   (also returned by \code{\link{classificationEndpoint}}). It is used in
#'   the SPARQL query together with \code{prefix} to select the target scheme.
#'
#' @param language Character string giving the language code for level labels.
#'   Defaults to \code{"en"}.
#'
#' @param showQuery Logical.  
#'   If \code{FALSE} (default), only the resulting data frame is returned.  
#'   If \code{TRUE}, the function returns a list containing:
#'   \itemize{
#'     \item \code{SPARQL.query}: the full query text,
#'     \item \code{dataStructure}: the resulting table.
#'   }
#'
#' @details
#' Before executing any SPARQL query, the function performs a pre-check using
#' \code{\link{classificationEndpoint}} to ensure that the combination
#' (\code{prefix}, \code{conceptScheme}) exists for the selected endpoint.
#'
#' If the combination does not exist, or if the endpoint is unavailable,
#' the function stops with a clear error message *before* issuing any SPARQL request.
#'
#' The behaviour of this function is also affected by the global option
#' \code{useLocalDataForVignettes}.  
#' If set to \code{TRUE}, a local CSV file is returned instead of querying
#' live endpoints. This is useful for offline use or vignette building.
#'
#' @return
#' A data frame containing one row per level of the classification, with
#' the following columns:
#' \itemize{
#'   \item \code{Concept_Scheme} – name of the classification scheme,
#'   \item \code{Depth} – hierarchical depth of the level,
#'   \item \code{Level} – label of the level (in the selected language),
#'   \item \code{Count} – number of categories at this level.
#' }
#'
#' If \code{showQuery = TRUE}, a list is returned instead (see details).
#'
#' @examples
#' \dontrun{
#' # Minimal example (CELLAR)
#' dataStructure("CELLAR", prefix = "nace2", conceptScheme = "nace2")
#'
#' # Loop over all CELLAR schemes
#' list_data <- classificationEndpoint("CELLAR")
#' res <- list()
#' for (i in seq_len(nrow(list_data))) {
#'   p  <- list_data$Prefix[i]
#'   cs <- list_data$ConceptScheme[i]
#'   res[[p]] <- dataStructure("CELLAR", prefix = p, conceptScheme = cs)
#' }
#' }
#'
#' @seealso
#' \code{\link{classificationEndpoint}},
#' \code{\link{retrieveClassificationTable}}.
#'
#' @import httr
#' @import jsonlite
#' @export


  

dataStructure = function(endpoint, prefix, conceptScheme, language = "en", showQuery = FALSE) {
  #Check correctness of endpoint argument
  endpoint <- toupper(endpoint)
  if (!endpoint %in% c("CELLAR", "FAO")) {
    stop("`endpoint` must be either 'CELLAR' or 'FAO'.")
  }
  # Check the useLocalDataForVignettes option
  if (getOption("useLocalDataForVignettes", FALSE)) {
    
    localDataPath <- system.file("extdata", paste0("dataStructure_", prefix, ".csv"), package = "correspondenceTables")
    
    if (file.exists(localDataPath)) {
      # Read data from the local file if it exists
      data <- read.csv(localDataPath)
      if (showQuery) {
        print("Data loaded from local file.")
      }
      return(data)
    }
  } else {
  
    ### Try to load endpoint configuration from GitHub
    config <- tryCatch(
      jsonlite::fromJSON(
        "https://raw.githubusercontent.com/eurostat/correspondenceTables/main/inst/extdata/endpoint_source_config.json"
      ),
      error = function(e) NULL
    )
    
    ### Default hardcoded endpoints
    default_endpoints <- list(
      CELLAR = "http://publications.europa.eu/webapi/rdf/sparql",
      FAO    = "https://stats.fao.org/caliper/sparql/AllVocs"
    )
    
    ### Select source endpoint
    if (!is.null(config)) {
      source <- config[[endpoint]]
    } else {
      source <- default_endpoints[[endpoint]]
    }
    
    ### URL base (unchanged)
    if (endpoint == "CELLAR") url <- "data.europa.eu/"
    if (endpoint == "FAO")    url <- "unstats.un.org/"
    
    # --- Pre-check with classificationEndpoint() before running SPARQL ----
    # (1) Try to list available schemes for this endpoint
    ce_res <- tryCatch(
      classificationEndpoint(endpoint),
      error = function(e) {
        stop(simpleError(
          paste0(
            "classificationEndpoint() failed when called from dataStructure(",
            "endpoint = '", endpoint, "', ",
            "prefix = '", prefix, "', ",
            "conceptScheme = '", conceptScheme, "'): ",
            conditionMessage(e)
          )
        ))
      }
    )
    
    # For a single endpoint, classificationEndpoint() should return a data.frame
    if (!is.data.frame(ce_res)) {
      stop(simpleError(
        paste0(
          "Unexpected object returned by classificationEndpoint('", endpoint,
          "') when called from dataStructure()."
        )
      ))
    }
    
    # (2b) Check that (prefix, conceptScheme) exists in classificationEndpoint()
    match_row <- ce_res$Prefix == prefix & ce_res$ConceptScheme == conceptScheme
    
    if (!any(match_row)) {
      # (3b) -> Do NOT run SPARQL, issue informative error
      stop(simpleError(
        paste0(
          "The combination (prefix = '", prefix, "', conceptScheme = '", conceptScheme,
          "', language = '", language, "') is not available for endpoint '", endpoint,
          "' according to classificationEndpoint(). No SPARQL call was made."
        )
      ))
    }
    
    # (3a) If we arrive here, the combination exists -> proceed with SPARQL
    
    
  ## Create Prefixes list 
  prefix_ls = prefixList(endpoint, prefix = prefix)
  prefix_ls = as.character(paste(prefix_ls, collapse = "\n"))
  ### Load prefixes from Excel file
  #prefix_file = read.csv(paste0("//lu-fsp01/Data_Lux/AgSTAT/Projects/CorrespondenceTables_Rpck/Task 3/prefix_", endpoint, ".csv"))
  #prefix = as.character(paste(prefix_file$List, collapse = "\n"))
  tryCatch(
    {
  ### SPARQL query
  SPARQL.query = paste0(prefix_ls, "
  SELECT  DISTINCT ?Concept_Scheme  ?Depth ?Level (COUNT (distinct ?s) AS ?Count) 
 

        WHERE {
          ?s skos:prefLabel ?Label ;
          skos:inScheme ", prefix, ":", conceptScheme, " ;
          skos:inScheme ?Scheme ;
          ^skos:member ?Member ;
          skos:prefLabel ?Label ;
          skos:notation ?notation .
          
          ?Member a xkos:ClassificationLevel .
          OPTIONAL {?member xkos:levels ?levels_temp . }
          OPTIONAL {?member xkos:depth ?Depth . }
        
          FILTER (?Scheme = ", prefix, ":", conceptScheme, ")
          FILTER (lang(?Label) = '", language, "')
          #FILTER (datatype(?notation) = rdf:PlainLiteral) 
        
          BIND (STR(?s) AS ?URL)
          #BIND (STR(?notation) AS ?CODE ) 
        
          BIND (STRAFTER(STR(", prefix, ":), 'http') AS ?CLASS_URL)
          #BIND (STRAFTER((?CLASS_URL), '/') AS ?Class)
          BIND (STRAFTER(STR(?Scheme), STR(?CLASS_URL)) AS ?Concept_Scheme)
          BIND (STRAFTER(str(?Member), STR(?CLASS_URL)) As ?Level)
         
          FILTER (STRLEN(?Concept_Scheme) != 0)
          FILTER (STRLEN(?Level) != 0)
  		    #FILTER (?Concept_Scheme != STR('ag'))
        }
        
      GROUP BY ?Concept_Scheme ?Depth ?Level
      ORDER BY ?Concept_Scheme ?Depth ?Level
      
  ")
  
  response = httr::POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
  table = read.csv(text=content(response, "text"), sep= ",")  
  table = table[order(table[,3],decreasing=FALSE),]
  
    }, error = function(e) {
      stop(simpleError(paste0("Error in function datastructure('", endpoint,"', '", prefix,"', '", conceptScheme,"'), endpoint is not available or is returning unexpected data\n")))
      cat("The following SPARQL code was used in the call:\n", SPARQL.query, "\n")
      cat("The following response was given for by the SPARQL call:\n", response)
    })

  if (nrow(table) == 0){
     message("This classification has no level. Please use level = 'ALL' when retrieving it using the retrieveClassificationTable")
  }
  
  if (showQuery) {
    result=list()
    result[[1]]= SPARQL.query
    result[[2]]= table
    names(result)=c("SPARQL.query", "dataStructure")
    cat(result$SPARQL.query, sep = "\n")
    return(result)
  } else {
    return(table)
  }
  
 }
}
