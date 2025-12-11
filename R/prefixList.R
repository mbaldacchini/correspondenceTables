prefixList <- function(endpoint, prefix = NULL) {
  endpoint <- toupper(endpoint)
  if (!endpoint %in% c("CELLAR", "FAO")) {
    stop("`endpoint` must be either 'CELLAR' or 'FAO'.")
  }
  
  prefix_init <- as.matrix(rbind(
    "PREFIX dc: <http://purl.org/dc/elements/1.1/>",
    "PREFIX dct: <http://purl.org/dc/terms/>",
    "PREFIX cb: <http://cbasewrap.ontologycentral.com/vocab#>",
    "PREFIX eli: <http://data.europa.eu/eli/ontology#>",
    "PREFIX euvoc: <http://publications.europa.eu/ontology/euvoc#>",
    "PREFIX owl: <http://www.w3.org/2002/07/owl#>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX skosxl: <http://www.w3.org/2008/05/skos-xl#>",
    "PREFIX xml: <http://www.w3.org/XML/1998/namespace>",
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
    "PREFIX is: <http://purl.org/ontology/is/core#>",
    "PREFIX isi: <http://purl.org/ontology/is/inst/>",
    "PREFIX cpc: <https://data.epo.org/linked-data/def/cpc/>",
    "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
    # "PREFIX CPC20: <https://unstats.un.org/classifications/CPC/v2.0/>",
    # "PREFIX CPC21: <https://unstats.un.org/classifications/CPC/v2.1/>"
  ))
  
  # --- Retrieve dynamic prefixes from classificationEndpoint() ---
  res <- classificationEndpoint(endpoint)
  
  # If classificationEndpoint() still returns a list, unpack it
  if (is.list(res) && !is.data.frame(res)) {
    if (length(res) == 1L) {
      res <- res[[1L]]
    } else {
      stop(
        "classificationEndpoint() returned multiple elements. ",
        "prefixList() expects a single endpoint ('CELLAR' or 'FAO')."
      )
    }
  }
  
  if (!is.data.frame(res) || !all(c("Prefix", "URI") %in% colnames(res))) {
    stop("Unexpected structure returned by classificationEndpoint().")
  }
  
  uri <- res$URI
  prefix_endpoint <- gsub("\\.", "", res$Prefix)
  
  dynamic_prefixes <- as.matrix(
    paste0("PREFIX ", prefix_endpoint, ": <", uri, "/>")
  )
  
  prefix_all <- rbind(prefix_init, dynamic_prefixes)
  prefix_all <- prefix_all[!duplicated(prefix_all), , drop = FALSE]
  
  if (!is.null(prefix)) {
    valid_prefixes <- prefix[prefix %in% prefix_endpoint]
    if (length(valid_prefixes) == 0) {
      stop("Desired prefixes not found for endpoint ", endpoint, ".")
    }
    
    uri_filtered <- uri[prefix_endpoint %in% valid_prefixes]
    filtered_matrix <- matrix(
      paste0("PREFIX ", valid_prefixes, ": <", uri_filtered, "/>")
    )
    
    prefix_all <- rbind(prefix_init, filtered_matrix)
  }
  
  return(prefix_all)
}
