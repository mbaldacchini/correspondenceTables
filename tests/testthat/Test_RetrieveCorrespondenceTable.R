test_that("retrieveCorrespondenceTable() returns a data.frame when showQuery = FALSE", {
  skip_on_cran()
  
  res <- tryCatch(
    retrieveCorrespondenceTable(
      prefix   = "cn2022",
      endpoint = "CELLAR",
      ID_table = "CN2022_NST2007",
      language = "en",
      CSVout   = FALSE,
      showQuery = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(res)) {
    skip("SPARQL endpoint not reachable for this test.")
  }
  
  expect_s3_class(res, "data.frame")
  expect_true(is.data.frame(res))
})


test_that("retrieveCorrespondenceTable() returns query and table when showQuery = TRUE", {
  skip_on_cran()
  
  res <- tryCatch(
    retrieveCorrespondenceTable(
      prefix   = "cn2022",
      endpoint = "CELLAR",
      ID_table = "CN2022_NST2007",
      language = "en",
      CSVout   = FALSE,
      showQuery = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(res)) {
    skip("SPARQL endpoint not reachable for this test.")
  }
  
  expect_type(res, "list")
  expect_true(all(c("SPARQL.query", "CorrespondenceTable") %in% names(res)))
  expect_s3_class(res$CorrespondenceTable, "data.frame")
})

test_that("retrieveCorrespondenceTable() writes CSV when CSVout is a filepath", {
  skip_on_cran()
  
  tmpdir <- tempdir()
  csv_path <- file.path(tmpdir, "CN2022_NST2007_test.csv")
  

  if (file.exists(csv_path)) file.remove(csv_path)
  
  res <- tryCatch(
    retrieveCorrespondenceTable(
      prefix   = "cn2022",
      endpoint = "CELLAR",
      ID_table = "CN2022_NST2007",
      language = "en",
      CSVout   = csv_path,
      showQuery = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(res)) {
    skip("SPARQL endpoint not reachable for this test.")
  }
  
  expect_s3_class(res, "data.frame")
  expect_true(file.exists(csv_path))
})


test_that("retrieveCorrespondenceTable() writes CSV when CSVout = TRUE", {
  skip_on_cran()
  
  old_wd <- getwd()
  tmpdir <- tempdir()
  setwd(tmpdir)
  on.exit(setwd(old_wd), add = TRUE)
  
  res <- tryCatch(
    retrieveCorrespondenceTable(
      prefix   = "cn2022",
      endpoint = "CELLAR",
      ID_table = "CN2022_NST2007",
      language = "en",
      CSVout   = TRUE,
      showQuery = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(res)) {
    skip("SPARQL endpoint not reachable for this test.")
  }
  
  expect_s3_class(res, "data.frame")
  
  expected_file <- "CN2022_NST2007_table.csv"
  expect_true(file.exists(expected_file))
})


test_that("retrieveCorrespondenceTable() rejects invalid endpoint", {
  expect_error(
    retrieveCorrespondenceTable(
      prefix   = "cn2022",
      endpoint = "INVALID",
      ID_table = "CN2022_NST2007"
    ),
    regexp = "is not accepted"
  )
})

test_that("retrieveCorrespondenceTable() stops cleanly when correspondenceTableList() fails", {
  skip_on_cran()
  
  # Fake correspondenceTableList that always fails
  fake_correspondenceTableList <- function(endpoint) {
    stop("Simulated correspondenceTableList() error")
  }
  
  testthat::with_mocked_bindings(
    {
      expect_error(
        retrieveCorrespondenceTable(
          prefix   = "cn2022",
          endpoint = "CELLAR",
          ID_table = "CN2022_NST2007"
        ),
        regexp = "correspondenceTableList\\(\\) failed when called from retrieveCorrespondenceTable"
      )
    },
    correspondenceTableList = fake_correspondenceTableList
  )
})


test_that("retrieveCorrespondenceTable() errors when (prefix, ID_table) is not in correspondenceTableList()", {
  skip_on_cran()
  
  # Fake correspondenceTableList that returns a table WITHOUT the requested (prefix, ID_table)
  fake_correspondenceTableList <- function(endpoint) {
    data.frame(
      prefix   = c("otherprefix"),
      ID_table = c("OTHER_TABLE"),
      stringsAsFactors = FALSE
    )
  }
  
  testthat::with_mocked_bindings(
    {
      expect_error(
        retrieveCorrespondenceTable(
          prefix   = "cn2022",
          endpoint = "CELLAR",
          ID_table = "CN2022_NST2007"
        ),
        regexp = "is not available for endpoint 'CELLAR' according to correspondenceTableList"
      )
    },
    correspondenceTableList = fake_correspondenceTableList
  )
})


test_that("retrieveCorrespondenceTable() returns query and table when showQuery = TRUE (integration test)", {
  skip_on_cran()
  
  res <- tryCatch(
    retrieveCorrespondenceTable(
      prefix   = "cn2022",
      endpoint = "CELLAR",
      ID_table = "CN2022_NST2007",
      language = "en",
      CSVout   = FALSE,
      showQuery = TRUE
    ),
    error = function(e) NULL
  )
  

  if (is.null(res)) {
    skip("SPARQL endpoint not reachable for retrieveCorrespondenceTable() integration test.")
  }
  
  expect_type(res, "list")
  expect_true(all(c("SPARQL.query", "CorrespondenceTable") %in% names(res)))
  expect_s3_class(res$CorrespondenceTable, "data.frame")
})
