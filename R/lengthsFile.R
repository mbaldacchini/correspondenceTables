#' Internal helper to derive code segment boundaries by level
#'
#' This internal function computes, for a given classification, the character
#' positions corresponding to each hierarchical level within the compact code
#' (i.e. without dots and spaces). It is used by higher-level routines and is
#' not part of the public API.
#'
#' @param endpoint Character; SPARQL endpoint. One of \code{"CELLAR"} or
#'   \code{"FAO"}.
#' @param prefix Character; prefix identifying the classification in the
#'   endpoint (e.g. \code{"nace2"}, \code{"cn2022"}).
#' @param conceptScheme Character; concept scheme identifier used in the
#'   SPARQL calls.
#' @param correction Boolean; if \code{TRUE} (default), applies
#'   classification-specific corrections that are used in production.
#'
#' @return A data.frame with two columns:
#' \itemize{
#'   \item \code{charb}: starting character position (1-based) of the segment
#'         corresponding to each hierarchical level (in the compact code),
#'   \item \code{chare}: ending character position (1-based) of that segment.
#' }
#'
#' @keywords internal
#' @noRd
lengthsFile <- function(endpoint, prefix, conceptScheme, correction = TRUE) {
  # Normalise endpoint / prefix for internal use
  endpoint <- toupper(endpoint)
  prefix   <- tolower(prefix)
  
  if (!is.logical(correction) || length(correction) != 1L) {
    stop("`correction` must be a single Boolean value (TRUE or FALSE).")
  }
  
  ## 1. Retrieve level metadata ---------------------------------------------
  level_dt <- dataStructure(endpoint, prefix, conceptScheme)
  
  if (isTRUE(correction)) {
    # order (for PRODCO)
    if (prefix %in% c("prodcom2019", "prodcom2021", "prodcom2022")) {
      level_dt <- level_dt[c(2, 1, 3), , drop = FALSE]
    }
    # remove first level (for CN)
    if (prefix %in% c("cn2017", "cn2018", "cn2019", "cn2020",
                      "cn2021", "cn2022", "cn2023")) {
      level_dt <- level_dt[-1, , drop = FALSE]
    }
    # order (for CPA)
    if (prefix %in% c("cpa21")) {
      level_dt <- level_dt[c(1, 2, 3, 6, 4, 5), , drop = FALSE]
    }
  }
  
  # Pour toutes les autres classifications, on s'assure que les niveaux
  # sont dans l'ordre croissant de la colonne 2 (profondeur hiérarchique)
  special_prefix <- c("prodcom2019", "prodcom2021", "prodcom2022",
                      "cn2017", "cn2018", "cn2019", "cn2020",
                      "cn2021", "cn2022", "cn2023",
                      "cpa21")
  if (!prefix %in% special_prefix) {
    ord <- order(suppressWarnings(as.numeric(level_dt[, 2])))
    level_dt <- level_dt[ord, , drop = FALSE]
  }
  
  nL <- nrow(level_dt)
  if (nL == 0L) {
    stop("dataStructure() returned no levels for this classification.")
  }
  
  # On suppose que la deuxième colonne de level_dt contient l'identifiant de niveau
  level_ids <- level_dt[, 2]
  
  # Vecteurs pour stocker les longueurs et positions
  total_len <- rep(NA_integer_, nL)
  charb     <- rep(NA_integer_, nL)
  chare     <- rep(NA_integer_, nL)
  
  ## 2. Boucle sur les niveaux ----------------------------------------------
  for (l in seq_len(nL)) {
    
    dt <- retrieveClassificationTable(
      endpoint      = endpoint,
      prefix        = prefix,
      conceptScheme = conceptScheme,
      level         = level_ids[l]
    )$ClassificationTable
    
    # --- Corrections spécifiques -------------------------------------------
    if (isTRUE(correction)) {
      
      # ecoicop: remove ".0" for 10, 11, 12 at the first level only
      if (prefix %in% c("ecoicop") && l == 1L) {
        idx <- which(dt[, 1] %in% c("10.0", "11.0", "12.0"))
        if (length(idx) > 0L) {
          dt[idx, 1] <- c("10", "11", "12")
        }
      }
      
      # prodcom: remove odd codes 00.99.t and 00.99.z at first level
      if (prefix %in% c("prodcom2019", "prodcom2021", "prodcom2022") && l == 1L) {
        bad <- which(dt[, 1] %in% c("00.99.t", "00.99.z"))
        if (length(bad) > 0L) {
          dt <- dt[-bad, , drop = FALSE]
        }
      }
      
      # NACE / NACE 2.1 / CPA / ISIC:
      # dans ta version de base, la lettre "A" n'était ajoutée
      # qu'à partir des niveaux > 1
      if (prefix %in% c("nace2", "nace21", "cpa21", "isicrev4") && l > 1L) {
        dt[, 1] <- paste0("A", dt[, 1])
      }
      
      # ICC_v11: add leading zero at level 2 uniquement
      if (prefix %in% c("icc_v11") && l == 2L) {
        dt[, 1] <- sprintf("%.2f", dt[, 1])
      }
    }
    
    # 3. Nettoyer les codes : on enlève espaces et points -------------------
    codes       <- as.character(dt[, 1])
    codes_clean <- gsub("[ .]", "", codes)
    
    len_unique <- unique(nchar(codes_clean))
    
    if (length(len_unique) != 1L || is.na(len_unique[1L])) {
      total_len[l] <- NA_integer_
      charb[l]     <- NA_integer_
      chare[l]     <- NA_integer_
      
      warning(
        "Inconsistent code lengths at level ", level_ids[l],
        " for prefix ", prefix,
        ". The lengths file may not be reliable for this level."
      )
      
    } else {
      total_len[l] <- len_unique[1L]
      
      if (l == 1L) {
        charb[l] <- 1L
        chare[l] <- total_len[l]
      } else {
        if (is.na(total_len[l - 1L]) || total_len[l] <= total_len[l - 1L]) {
          charb[l] <- NA_integer_
          chare[l] <- NA_integer_
          
          warning(
            "Non-increasing or invalid code length from level ",
            level_ids[l - 1L], " to level ", level_ids[l],
            " for prefix ", prefix,
            ". The lengths file may not be reliable for this level."
          )
          
        } else {
          seg_len   <- total_len[l] - total_len[l - 1L]
          charb[l]  <- chare[l - 1L] + 1L
          chare[l]  <- chare[l - 1L] + seg_len
        }
      }
    }
  }
  
  ## 3. Table finale --------------------------------------------------------
  lengths <- data.frame(
    charb = charb,
    chare = chare,
    stringsAsFactors = FALSE
  )
  
  if (any(is.na(lengths$charb) | is.na(lengths$chare))) {
    warning(
      "There is a problem with the given classification; ",
      "the lengths file produced should not be fully trusted. ",
      "Please check the classification and correct any issue."
    )
  }
  
  if (isFALSE(correction)) {
    warning(
      "The lengths file produced could be wrong. ",
      "Please make sure the classification is correct."
    )
  }
  
  lengths
}
