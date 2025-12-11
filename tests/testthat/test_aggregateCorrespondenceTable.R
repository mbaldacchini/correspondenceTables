
test_that("aggregateCorrespondenceTable returns correct result for normal input", {
  AB <- system.file("extdata/test", "ab_data.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  ACT_normalT <- aggregateCorrespondenceTable(AB, A, B, NULL)
  
  ACText_normalT <- system.file("extdata/test", "AGTresult1.csv", package = "correspondenceTables")
  ACText_normalT <- utils::read.csv(ACText_normalT, sep = ",", header = TRUE, check.names = FALSE,
                                    colClasses = c("character"), encoding = "UTF-8")
  
  expect_equal(ACT_normalT, ACText_normalT)
})

test_that("aggregateCorrespondenceTable errors on duplicate data", {
  AB <- system.file("extdata/test", "ab_data_duplicate.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  expect_error(aggregateCorrespondenceTable(AB, A, B, NULL))
})
 
test_that("aggregateCorrespondenceTable errors on empty data", {
  AB <- system.file("extdata/test", "ab_data_empty.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  expect_error(aggregateCorrespondenceTable(AB, A, B, NULL))
})

test_that("aggregateCorrespondenceTable errors on missing code", {
  AB <- system.file("extdata/test", "ab_data_missingcode.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  expect_error(aggregateCorrespondenceTable(AB, A, B, NULL))
})

test_that("aggregateCorrespondenceTable errors on text suppression issues", {
  AB <- system.file("extdata/test", "ab_data.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "a_data_textsup.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "b_data.csv", package = "correspondenceTables")
  
  expect_error(aggregateCorrespondenceTable(AB, A, B, NULL))
})

test_that("aggregateCorrespondenceTable accepts both data.frames and CSV paths", {
  skip_on_cran()
  
  # --- 1. Create minimal consistent example --------------------------
  
  # Source classification A: 2-level hierarchy
  A_df <- data.frame(
    Acode     = c("A1",  "A2"),
    Alevel    = c(1,     2),
    Asuperior = c(NA,   "A1"),
    stringsAsFactors = FALSE
  )
  
  # Target classification B: 2-level hierarchy
  B_df <- data.frame(
    Bcode     = c("B1",  "B2"),
    Blevel    = c(1,     2),
    Bsuperior = c(NA,   "B1"),
    stringsAsFactors = FALSE
  )
  
  # Correspondence AB at most granular level
  AB_df <- data.frame(
    Acode = "A2",
    Bcode = "B2",
    stringsAsFactors = FALSE
  )
  
  # --- 2. Write temporary CSV files ---------------------------------
  
  AB_path <- tempfile(fileext = ".csv")
  A_path  <- tempfile(fileext = ".csv")
  B_path  <- tempfile(fileext = ".csv")
  
  utils::write.csv(AB_df, AB_path, row.names = FALSE)
  utils::write.csv(A_df,  A_path,  row.names = FALSE)
  utils::write.csv(B_df,  B_path,  row.names = FALSE)
  
  # --- 3. All arguments as CSV paths --------------------------------
  
  expect_error(
    res_csv <- aggregateCorrespondenceTable(
      AB     = AB_path,
      A      = A_path,
      B      = B_path,
      CSVout = NULL
    ),
    NA
  )
  
  expect_s3_class(res_csv, "data.frame")
  expect_gt(nrow(res_csv), 0)
  
  # --- 4. All arguments as data.frames -------------------------------
  
  expect_error(
    res_df <- aggregateCorrespondenceTable(
      AB     = AB_df,
      A      = A_df,
      B      = B_df,
      CSVout = NULL
    ),
    NA
  )
  
  expect_s3_class(res_df, "data.frame")
  expect_gt(nrow(res_df), 0)
  
  # --- 5. Mixed formats: AB data.frame, A/B CSV ---------------------
  
  expect_error(
    res_mixed1 <- aggregateCorrespondenceTable(
      AB     = AB_df,
      A      = A_path,
      B      = B_path,
      CSVout = NULL
    ),
    NA
  )
  
  expect_s3_class(res_mixed1, "data.frame")
  expect_gt(nrow(res_mixed1), 0)
  
  # --- 6. Mixed formats: AB CSV, A/B data.frame ---------------------
  
  expect_error(
    res_mixed2 <- aggregateCorrespondenceTable(
      AB     = AB_path,
      A      = A_df,
      B      = B_df,
      CSVout = NULL
    ),
    NA
  )
  
  expect_s3_class(res_mixed2, "data.frame")
  expect_gt(nrow(res_mixed2), 0)
  
  # --- 7. Check CSVout actually writes a file -----------------------
  
  tmp_out <- tempfile(fileext = ".csv")
  
  expect_error(
    res_out <- aggregateCorrespondenceTable(
      AB     = AB_df,
      A      = A_df,
      B      = B_df,
      CSVout = tmp_out
    ),
    NA
  )
  
  expect_true(file.exists(tmp_out))
  expect_s3_class(res_out, "data.frame")
  expect_gt(nrow(res_out), 0)
  
  unlink(c(AB_path, A_path, B_path, tmp_out))
})
  
