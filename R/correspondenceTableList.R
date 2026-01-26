#' @title Retrieve a list of all correspondence tables in the CELLAR and FAO repositories
#'
#' @description
#' Lists all correspondence tables (XKOS correspondences) in the selected endpoint(s).
#' The function relies on \code{classificationList()} to obtain the set of known
#' classification schemes, then retrieves correspondences that reference those schemes.
#'
#' @param endpoint Character. SPARQL endpoint(s) to query. One of:
#'   \code{"CELLAR"}, \code{"FAO"}, or \code{"ALL"} (default).
#' @param showQuery Logical; if \code{TRUE}, prints the SPARQL query used.
#'   Default is \code{FALSE}.
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
#' If \code{endpoint = "ALL"}, returns a named list with elements \code{$CELLAR}
#' and \code{$FAO}, each a \code{data.frame} as above.
#'
#' @details
#' Behaviour depends on the global option \code{useLocalDataForVignettes}:
#' \itemize{
#'   \item If \code{TRUE}: reads pre-embedded CSVs in \code{inst/extdata}.
#'   \item If \code{FALSE}: runs live SPARQL queries.
#' }
#'
#' @import httr
#' @import jsonlite
#' @export
#'
#' @examples
#' if (interactive()) {
#'   out <- correspondenceTableList("ALL")
#'   head(out$CELLAR)
#'   head(out$FAO)
#' }


correspondenceTableList <- function(endpoint = "ALL", showQuery = FALSE) {
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  
  # ---------- Offline / vignette mode ----------
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
      stop(simpleError(paste0(
        "Error in correspondenceTableList('", endpoint, "'): ",
        "expected local CSV at ", path, " but it was not found."
      )))
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE)
    return(df)
  }
  
  # ---------- Online mode ----------
  cfg <- tryCatch(
    jsonlite::fromJSON(
      "https://raw.githubusercontent.com/eurostat/correspondenceTables/refs/heads/main/inst/extdata/endpoint_source_config.json"
    ),
    error = function(e) NULL
  )
  default_endpoints <- list(
    CELLAR = "https://publications.europa.eu/webapi/rdf/sparql",
    FAO    = "https://caliper.integratedmodelling.org/caliper/sparql/"
  )
  
  # Helper to run one endpoint
  run_one <- function(ep) {
    src <- if (!is.null(cfg) && !is.null(cfg[[ep]])) cfg[[ep]] else default_endpoints[[ep]]
    
    # ---- 1) Get the catalog of schemes ----
    schemes <- tryCatch(
      classificationList(endpoint = ep),
      error = function(e) {
        stop(simpleError(paste0(
          "Error in correspondenceTableList('", ep, "'): classificationList() failed.\n",
          "Original error: ", conditionMessage(e)
        )))
      }
    )
    
    if (!is.data.frame(schemes) || nrow(schemes) == 0L) {
      return(data.frame(
        Prefix                 = character(),
        ID                     = character(),
        Source.Classification  = character(),
        Target.Classification  = character(),
        Table.Name             = character(),
        URI                    = character(),
        stringsAsFactors       = FALSE
      ))
    }
    
    # ---- 2) Extract scheme URIs ----
    uri_cols <- intersect(names(schemes),
                          c("SchemeURI","ConceptSchemeURI","Scheme",
                            "Concept.Scheme.URI","Concept.Scheme","URI"))
    has_ns_cs <- all(c("Namespace","ConceptScheme") %in% names(schemes))
    
    get_scheme_uri <- function(row) {
      for (nm in uri_cols) {
        v <- row[[nm]]
        if (is.character(v) && length(v) == 1L && grepl("^https?://", v)) return(v)
      }
      if (has_ns_cs) {
        ns <- row[["Namespace"]]
        cs <- row[["ConceptScheme"]]
        if (is.character(ns) && is.character(cs) &&
            nzchar(ns) && nzchar(cs) && grepl("^https?://", ns)) {
          ns2 <- if (endsWith(ns, "/")) ns else paste0(ns, "/")
          return(paste0(ns2, cs, "/scheme"))
        }
      }
      NA_character_
    }
    
    scheme_uri <- vapply(seq_len(nrow(schemes)),
                         function(i) get_scheme_uri(schemes[i, , drop = FALSE]),
                         character(1))
    scheme_uri <- unique(scheme_uri[!is.na(scheme_uri) & nzchar(scheme_uri)])
    
    if (length(scheme_uri) == 0L) {
      stop(simpleError(paste0(
        "Error in correspondenceTableList('", ep, "'): ",
        "classificationList() did not provide any scheme URIs to filter correspondences."
      )))
    }
    
    # Map SchemeURI -> Prefix
    prefix_map <- list()
    if ("Prefix" %in% names(schemes)) {
      idx <- match(scheme_uri, vapply(seq_len(nrow(schemes)),
                                      function(i) get_scheme_uri(schemes[i, , drop = FALSE]),
                                      character(1)))
      prefix_map <- stats::setNames(schemes$Prefix[idx], scheme_uri)
    } else {
      prefix_map <- stats::setNames(rep("", length(scheme_uri)), scheme_uri)
    }
    
    # ---- 3) Build relaxed query + chunked VALUES + fallback ----
    make_values <- function(uris) {
      paste0("VALUES ?KnownScheme { ", paste(sprintf("<%s>", uris), collapse = " "), " }")
    }
    prefix_header <- paste(
      "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
      "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
      sep = "\n"
    )
    
    # Run the relaxed query in chunks to avoid overly long VALUES
    values_chunk_size <- 200L
    n <- length(scheme_uri)
    chunk_starts <- seq(1L, n, by = values_chunk_size)
    dfs <- list()
    
    for (cs in chunk_starts) {
      ce <- min(cs + values_chunk_size - 1L, n)
      uris_chunk <- scheme_uri[cs:ce]
      values_block <- make_values(uris_chunk)
      
      ### FAO: query che estrae i token dagli URI (niente inScheme/inCorrespondence)
      if (ep == "FAO") {
        SPARQL.query <- paste0(
          prefix_header, "\n",
          "SELECT DISTINCT ?corr ?Label ?srcTok ?tgtTok\n",
          "WHERE {\n",
          "  ?corr a xkos:Correspondence .\n",
          "  OPTIONAL { ?corr skos:prefLabel ?Label . }\n",
          "  # ID = basename(URI)\n",
          "  BIND(REPLACE(STR(?corr), \".*/\", \"\") AS ?_id)\n",
          "  # srcTok = parte sinistra di '--' oppure, se assente, prima di '-'\n",
          "  BIND(IF(CONTAINS(?_id, \"--\"), REPLACE(?_id, \"^(.+?)--.+$\", \"$1\"), REPLACE(?_id, \"^(.+?)-.+$\", \"$1\")) AS ?srcTok)\n",
          "  # tgtTok = parte destra\n",
          "  BIND(IF(CONTAINS(?_id, \"--\"), REPLACE(?_id, \"^.+?--(.+)$\", \"$1\"), REPLACE(?_id, \"^.+?-(.+)$\", \"$1\")) AS ?tgtTok)\n",
          "}\n"
        )
      } else {
        # CELLAR: query originale
        SPARQL.query <- paste0(
          prefix_header, "\n",
          "SELECT DISTINCT ?corr ?Label ?srcScheme ?tgtScheme\n",
          "WHERE {\n",
          "  ", values_block, "\n",
          "  ?corr a xkos:Correspondence .\n",
          "  OPTIONAL { ?corr skos:prefLabel ?Label . }\n",
          "  OPTIONAL {\n",
          "    ?assoc xkos:inCorrespondence ?corr .\n",
          "    OPTIONAL { ?assoc xkos:sourceConcept ?src . ?src skos:inScheme ?srcScheme . }\n",
          "    OPTIONAL { ?assoc xkos:targetConcept ?tgt . ?tgt skos:inScheme ?tgtScheme . }\n",
          "  }\n",
          "}\n"
        )
      }
      
      if (isTRUE(showQuery)) {
        message("SPARQL query for endpoint = ", ep, " [chunk ", cs, "-", ce, "]:\n", SPARQL.query)
      }
      
      df_chunk <- tryCatch({
        resp <- httr::POST(
          url   = src,
          httr::accept("text/csv"),
          body  = list(query = SPARQL.query),
          encode = "form"
        )
        utils::read.csv(text = httr::content(resp, "text"), stringsAsFactors = FALSE)
      }, error = function(e) {
        stop(simpleError(paste0(
          "Error in correspondenceTableList('", ep, "'): SPARQL request failed.\n",
          "Original error: ", conditionMessage(e)
        )))
      })
      
      if (nrow(df_chunk)) dfs[[length(dfs) + 1L]] <- df_chunk
    }
    
    # If all chunked queries returned empty, try a minimal fallback without VALUES
    if (length(dfs) == 0L) {
      SPARQL.fallback <- if (ep == "FAO") {
        paste0(
          prefix_header, "\n",
          "SELECT DISTINCT ?corr ?Label ?srcTok ?tgtTok\n",
          "WHERE {\n",
          "  ?corr a xkos:Correspondence .\n",
          "  OPTIONAL { ?corr skos:prefLabel ?Label . }\n",
          "  BIND(REPLACE(STR(?corr), \".*/\", \"\") AS ?_id)\n",
          "  BIND(IF(CONTAINS(?_id, \"--\"), REPLACE(?_id, \"^(.+?)--.+$\", \"$1\"), REPLACE(?_id, \"^(.+?)-.+$\", \"$1\")) AS ?srcTok)\n",
          "  BIND(IF(CONTAINS(?_id, \"--\"), REPLACE(?_id, \"^.+?--(.+)$\", \"$1\"), REPLACE(?_id, \"^.+?-(.+)$\", \"$1\")) AS ?tgtTok)\n",
          "}\n"
        )
      } else {
        paste0(
          prefix_header, "\n",
          "SELECT DISTINCT ?corr ?Label\n",
          "WHERE {\n",
          "  ?corr a xkos:Correspondence .\n",
          "  OPTIONAL { ?corr skos:prefLabel ?Label . }\n",
          "}\n"
        )
      }
      if (isTRUE(showQuery)) {
        message("Fallback SPARQL query for endpoint = ", ep, ":\n", SPARQL.fallback)
      }
      resp2 <- httr::POST(src, httr::accept("text/csv"),
                          body = list(query = SPARQL.fallback), encode = "form")
      df <- utils::read.csv(text = httr::content(resp2, "text"), stringsAsFactors = FALSE)
    } else {
      # Bind chunked results
      df <- dfs[[1L]]
      if (length(dfs) > 1L) {
        for (k in 2:length(dfs)) {
          df <- tryCatch(
            {
              base::rbind(df, dfs[[k]])
            },
            error = function(e) {
              # different column orders: align names
              common <- intersect(names(df), names(dfs[[k]]))
              base::rbind(df[, common, drop = FALSE], dfs[[k]][, common, drop = FALSE])
            }
          )
        }
      }
    }
    
    # Normalize expected columns
    for (nm in c("corr","Label","srcScheme","tgtScheme","srcTok","tgtTok")) {
      if (!nm %in% names(df)) df[[nm]] <- ""
    }
    
    # ---- 4) Best-effort inference (tua logica originale) ----
    schemes$ConceptToken <- NA_character_
    if ("ConceptScheme" %in% names(schemes) && is.character(schemes$ConceptScheme)) {
      schemes$ConceptToken <- schemes$ConceptScheme
    } else if ("URI" %in% names(schemes) && is.character(schemes$URI)) {
      schemes$ConceptToken <- sub(".*/", "", schemes$URI)
    }
    
    scheme_uri_col <- if ("URI" %in% names(schemes)) {
      schemes$URI
    } else if ("SchemeURI" %in% names(schemes)) {
      schemes$SchemeURI
    } else if ("ConceptSchemeURI" %in% names(schemes)) {
      schemes$ConceptSchemeURI
    } else {
      rep(NA_character_, nrow(schemes))
    }
    scheme_lookup <- stats::setNames(scheme_uri_col, schemes$ConceptToken)
    
    infer_scheme_from_text <- function(txt) {
      if (!is.character(txt) || !nzchar(txt)) return(character(0))
      toks <- names(scheme_lookup)
      toks <- toks[!is.na(toks) & nzchar(toks)]
      hits <- toks[vapply(toks, function(tk) grepl(tk, txt, fixed = TRUE), logical(1))]
      unique(unname(scheme_lookup[hits]))
    }
    
    is_empty <- function(x) { is.na(x) | !nzchar(x) }
    if (nrow(df)) {
      for (i in seq_len(nrow(df))) {
        if (is_empty(df$srcScheme[i]) || is_empty(df$tgtScheme[i])) {
          s_uri <- infer_scheme_from_text(df$corr[i])
          s_lab <- infer_scheme_from_text(df$Label[i])
          inferred <- unique(c(s_uri, s_lab))
          if (length(inferred) >= 1L && is_empty(df$srcScheme[i])) df$srcScheme[i] <- inferred[1L]
          if (length(inferred) >= 2L && is_empty(df$tgtScheme[i])) df$tgtScheme[i] <- inferred[2L]
        }
      }
    }
    
    # ---- 5) Build output with expected columns ----
    pick_prefix <- function(src, tgt) {
      if (!is.na(src) && nzchar(src) && src %in% names(prefix_map)) {
        prefix_map[[src]]
      } else if (!is.na(tgt) && nzchar(tgt) && tgt %in% names(prefix_map)) {
        prefix_map[[tgt]]
      } else {
        ""
      }
    }
    
    out <- data.frame(
      Prefix                 = mapply(pick_prefix, df$srcScheme, df$tgtScheme, USE.NAMES = FALSE),
      ID                     = sub(".*/", "", df$corr),
      Source.Classification  = ifelse(is.na(df$srcScheme), "", df$srcScheme),
      Target.Classification  = ifelse(is.na(df$tgtScheme), "", df$tgtScheme),
      Table.Name             = ifelse(is.na(df$Label), "", df$Label),
      URI                    = ifelse(is.na(df$corr), "", df$corr),
      stringsAsFactors       = FALSE
    ) 
    
    # ### FAO: se Prefix è ancora vuoto, riempilo dai token estratti
    if (ep == "FAO" && nrow(df)) {
      to_pref <- function(tok) {
        tok <- tolower(trimws(tok))
        tok <- gsub("[^a-z0-9]+", "", tok)     # rimuove separatori
        tok <- gsub("^icc(v)?([0-9]+)$", "icc\\2", tok)
        tok <- gsub("^isic(v)?([0-9]+)$", "isic\\2", tok)
        tok <- gsub("^cpc(v)?([0-9]+)$", "cpc\\2", tok)
        tok
      }
      srcP <- to_pref(df$srcTok)
      tgtP <- to_pref(df$tgtTok)
      # scegliamo il prefisso del lato "sorgente" se presente, altrimenti quello "target"
      prefTok <- ifelse(nzchar(srcP), srcP, tgtP)
      empties <- which(!nzchar(out$Prefix))
      if (length(empties)) {
        out$Prefix[empties] <- prefTok[empties]
      }
    }
    
    # De-duplicate
    out <- out[!duplicated(out), , drop = FALSE]
    out
  }
  
  if (endpoint == "ALL") {
    return(list(
      CELLAR = run_one("CELLAR"),
      FAO    = run_one("FAO")
    ))
  } else {
    return(run_one(endpoint))
  }
}
