test_that("classificationEndpoint is a deprecated wrapper around classificationList", {
  skip_on_cran()
  
  # Check formals
  args <- names(formals(classificationEndpoint))
  expect_true(all(c("endpoint", "showQuery") %in% args))
  
  # We *expect* a deprecation warning here
  res1 <- NULL
  expect_warning(
    res1 <- classificationEndpoint("CELLAR", showQuery = FALSE),
    regexp = "deprecated"
  )
  
  # classificationList should return the same object
  res2 <- classificationList(endpoint = "CELLAR", showQuery = FALSE)
  
  expect_equal(res1, res2)
})
