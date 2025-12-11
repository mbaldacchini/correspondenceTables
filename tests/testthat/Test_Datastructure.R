test_that("dataStructure() validates the 'endpoint' argument", {
  # Invalid endpoint should raise an error
  expect_error(
    dataStructure(endpoint = "INVALID", prefix = "CPC20", conceptScheme = "CPC20"),
    regexp = "not accepted"
  )
})

test_that("dataStructure() returns a data.frame for CELLAR when online", {
  skip_on_cran()
  skip_if_offline()
  
  # Use classificationEndpoint() to obtain a valid pair (prefix, conceptScheme)
  endpoints <- classificationEndpoint("CELLAR")
  expect_s3_class(endpoints, "data.frame")
  expect_true(nrow(endpoints) >= 1L)
  
  prefix        <- endpoints$Prefix[1L]
  conceptScheme <- endpoints$ConceptScheme[1L]
  
  res <- dataStructure(
    endpoint      = "CELLAR",
    prefix        = prefix,
    conceptScheme = conceptScheme,
    language      = "en",
    showQuery     = FALSE
  )
  
  # Should always return a data.frame (possibly empty)
  expect_s3_class(res, "data.frame")
  expect_true(is.numeric(nrow(res)))
})


test_that("dataStructure() returns a data.frame for FAO when online", {
  skip_on_cran()
  skip_if_offline()
  
  endpoints <- classificationEndpoint("FAO")
  expect_s3_class(endpoints, "data.frame")
  expect_true(nrow(endpoints) >= 1L)
  
  prefix        <- endpoints$Prefix[1L]
  conceptScheme <- endpoints$ConceptScheme[1L]
  
  res <- dataStructure(
    endpoint      = "FAO",
    prefix        = prefix,
    conceptScheme = conceptScheme,
    language      = "en",
    showQuery     = FALSE
  )
  
  expect_s3_class(res, "data.frame")
  expect_true(is.numeric(nrow(res)))
})


test_that("dataStructure(showQuery = TRUE) returns query and data", {
  skip_on_cran()
  skip_if_offline()
  
  endpoints <- classificationEndpoint("CELLAR")
  expect_s3_class(endpoints, "data.frame")
  expect_true(nrow(endpoints) >= 1L)
  
  prefix        <- endpoints$Prefix[1L]
  conceptScheme <- endpoints$ConceptScheme[1L]
  
  res <- dataStructure(
    endpoint      = "CELLAR",
    prefix        = prefix,
    conceptScheme = conceptScheme,
    language      = "en",
    showQuery     = TRUE
  )
  
  expect_true(is.list(res))
  expect_true(all(c("SPARQL.query", "dataStructure") %in% names(res)))
  
  expect_type(res$SPARQL.query, "character")
  expect_s3_class(res$dataStructure, "data.frame")
})


test_that("dataStructure() integrates with classificationEndpoint() for a small subset", {
  skip_on_cran()
  skip_if_offline()
  
  endpoints <- classificationEndpoint("CELLAR")
  expect_s3_class(endpoints, "data.frame")
  expect_true(nrow(endpoints) >= 1L)
  
  n <- min(2L, nrow(endpoints))  # keep test fast
  endpoint <- "CELLAR"
  
  res_list <- vector("list", n)
  
  for (i in seq_len(n)) {
    prefix        <- endpoints$Prefix[i]
    conceptScheme <- endpoints$ConceptScheme[i]
    
    ds <- dataStructure(
      endpoint      = endpoint,
      prefix        = prefix,
      conceptScheme = conceptScheme,
      language      = "en"
    )
    
    expect_s3_class(ds, "data.frame")
    res_list[[i]] <- ds
  }
  
  expect_true(all(vapply(res_list, is.data.frame, logical(1))))
})

test_that("dataStructure() gives a clear error when FAO endpoint is unavailable", {
  skip_on_cran()
  
  # Temporarily force an invalid FAO endpoint for classificationEndpoint()
  old_opt <- getOption("correspondenceTables.fao_endpoint")
  on.exit(options(correspondenceTables.fao_endpoint = old_opt), add = TRUE)
  
  options(correspondenceTables.fao_endpoint =
            "https://this-fao-endpoint-should-not-exist-12345.org/sparql"
  )
  
  expect_error(
    suppressWarnings(
      dataStructure(
        endpoint      = "FAO",
        prefix        = "XXX",        # arbitrary
        conceptScheme = "YYY",        # arbitrary
        language      = "en"
      )
    ),
    regexp = "Endpoint 'FAO' is currently unavailable"
  )
})

test_that("dataStructure() errors when prefix/conceptScheme do not exist for CELLAR", {
  skip_on_cran()
  
  expect_error(
    suppressWarnings(
      dataStructure(
        endpoint      = "CELLAR",
        prefix        = "XXX",  # does not exist
        conceptScheme = "YYY",  # does not exist
        language      = "en"
      )
    ),
    regexp = "Desired prefixes not found for endpoint CELLAR"
  )
})

