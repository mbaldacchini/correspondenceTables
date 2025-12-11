test_that("retrieveClassificationTable() rejects invalid endpoint", {
  expect_error(
    retrieveClassificationTable(
      endpoint      = "INVALID",
      prefix        = "cn2022",
      conceptScheme = "cn2022"
    ),
    regexp = "is not accepted"
  )
})

test_that("retrieveClassificationTable() returns a data.frame when showQuery = FALSE", {
  skip_on_cran()
  
  res <- tryCatch(
    retrieveClassificationTable(
      endpoint      = "CELLAR",
      prefix        = "cn2022",
      conceptScheme = "cn2022",
      language      = "en",
      level         = "ALL",
      showQuery     = FALSE
    ),
    error = function(e) NULL
  )
  
  # Si l'appel réseau échoue, on ne fait pas échouer le test,
  # on le skippe gentiment.
  if (is.null(res)) {
    skip("SPARQL endpoint not reachable for this test.")
  }
  
  expect_s3_class(res, "data.frame")
  expect_true(all(c("cn2022", "Name") %in% names(res)))
})


test_that("retrieveClassificationTable() returns query and table when showQuery = TRUE", {
  skip_on_cran()
  
  res <- tryCatch(
    retrieveClassificationTable(
      endpoint      = "CELLAR",
      prefix        = "cn2022",
      conceptScheme = "cn2022",
      language      = "en",
      level         = "ALL",
      showQuery     = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(res)) {
    skip("SPARQL endpoint not reachable for this test.")
  }
  
  expect_type(res, "list")
  expect_true(all(c("SPARQL.query", "ClassificationTable") %in% names(res)))
  expect_s3_class(res$ClassificationTable, "data.frame")
})



test_that("retrieveClassificationTable() validates the endpoint argument", {
  expect_error(
    retrieveClassificationTable(
      endpoint      = "INVALID",
      prefix        = "cn2022",
      conceptScheme = "cn2022"
    ),
    regexp = "is not accepted"
  )
})


test_that("retrieveClassificationTable() stops cleanly when dataStructure() fails", {
  skip_on_cran()
  
  fake_dataStructure <- function(endpoint, prefix, conceptScheme, language) {
    stop("Simulated dataStructure() error")
  }
  
  fake_prefixList <- function(endpoint, prefix = NULL) {
    matrix("PREFIX dummy: <http://example.com/>", ncol = 1)
  }
  
  fake_CsvFileSave <- function(CSVout, data) {
    invisible(NULL)
  }
  
  testthat::with_mocked_bindings(
    {
      expect_error(
        retrieveClassificationTable(
          endpoint      = "CELLAR",
          prefix        = "cn2022",
          conceptScheme = "cn2022"
        ),
        regexp = "dataStructure\\(\\) failed"
      )
    },
    dataStructure = fake_dataStructure,
    prefixList    = fake_prefixList,
    CsvFileSave   = fake_CsvFileSave
  )
})

test_that("retrieveClassificationTable() errors if classification has no levels and level != 'ALL'", {
  skip_on_cran()
  
  # dataStructure() returns an empty data.frame
  fake_dataStructure <- function(endpoint, prefix, conceptScheme, language) {
    data.frame(
      Concept_Scheme = character(),
      Depth          = numeric(),
      Level          = character(),
      Count          = integer(),
      stringsAsFactors = FALSE
    )
  }
  
  fake_prefixList <- function(endpoint, prefix = NULL) {
    matrix("PREFIX dummy: <http://example.com/>", ncol = 1)
  }
  
  fake_CsvFileSave <- function(CSVout, data) {
    invisible(NULL)
  }
  
  testthat::with_mocked_bindings(
    {
      expect_error(
        retrieveClassificationTable(
          endpoint      = "CELLAR",
          prefix        = "cn2022",
          conceptScheme = "cn2022",
          level         = "2"
        ),
        regexp = "has no level information"
      )
    },
    dataStructure = fake_dataStructure,
    prefixList    = fake_prefixList,
    CsvFileSave   = fake_CsvFileSave
  )
})


test_that("retrieveClassificationTable() errors if requested level does not exist", {
  skip_on_cran()
  
  fake_dataStructure <- function(endpoint, prefix, conceptScheme, language) {
    data.frame(
      Concept_Scheme = c(conceptScheme, conceptScheme),
      Depth          = c(1, 2),
      Level          = c("Level 1", "Level 2"),
      Count          = c(10, 20),
      stringsAsFactors = FALSE
    )
  }
  
  fake_prefixList <- function(endpoint, prefix = NULL) {
    matrix("PREFIX dummy: <http://example.com/>", ncol = 1)
  }
  
  fake_CsvFileSave <- function(CSVout, data) {
    invisible(NULL)
  }
  
  testthat::with_mocked_bindings(
    {
      expect_error(
        retrieveClassificationTable(
          endpoint      = "CELLAR",
          prefix        = "cn2022",
          conceptScheme = "cn2022",
          level         = "3"
        ),
        regexp = "Requested level '3' does not exist"
      )
    },
    dataStructure = fake_dataStructure,
    prefixList    = fake_prefixList,
    CsvFileSave   = fake_CsvFileSave
  )
})

test_that("retrieveClassificationTable() errors when level is non-numeric and not 'ALL'", {
  skip_on_cran()
  
  fake_dataStructure <- function(endpoint, prefix, conceptScheme, language) {
    data.frame(
      Concept_Scheme = conceptScheme,
      Depth          = c(1, 2),
      Level          = c("Level 1", "Level 2"),
      Count          = c(10, 20),
      stringsAsFactors = FALSE
    )
  }
  
  fake_prefixList <- function(endpoint, prefix = NULL) {
    matrix("PREFIX dummy: <http://example.com/>", ncol = 1)
  }
  
  fake_CsvFileSave <- function(CSVout, data) {
    invisible(NULL)
  }
  
  testthat::with_mocked_bindings(
    {
      expect_error(
        retrieveClassificationTable(
          endpoint      = "CELLAR",
          prefix        = "cn2022",
          conceptScheme = "cn2022",
          level         = "DIVISION"
        ),
        regexp = "is not numeric"
      )
    },
    dataStructure = fake_dataStructure,
    prefixList    = fake_prefixList,
    CsvFileSave   = fake_CsvFileSave
  )
})
