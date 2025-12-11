test_that("prefixList() validates the 'endpoint' argument", {
  expect_error(
    prefixList("ALL"),
    regexp = "`endpoint` must be either 'CELLAR' or 'FAO'."
  )
  
  expect_error(
    prefixList("INVALID"),
    regexp = "`endpoint` must be either 'CELLAR' or 'FAO'."
  )
  
  # Should not error for valid endpoints (even if offline)
  expect_error(prefixList("CELLAR"), NA)
  expect_error(prefixList("FAO"), NA)
})

test_that("prefixList('CELLAR') returns a character matrix when online", {
  skip_on_cran()
  skip_if_offline()
  
  res <- prefixList("CELLAR")
  
  # Basic structure checks
  expect_true(is.matrix(res))
  expect_true(is.character(res))
  expect_true(nrow(res) >= 1L)
  
  # Every row should look like a PREFIX declaration
  expect_true(all(grepl("^PREFIX\\s+", res[, 1])))
})

test_that("prefixList('FAO') returns a character matrix when online", {
  skip_on_cran()
  skip_if_offline()
  
  res <- prefixList("FAO")
  
  expect_true(is.matrix(res))
  expect_true(is.character(res))
  expect_true(nrow(res) >= 1L)
  expect_true(all(grepl("^PREFIX\\s+", res[, 1])))
})


test_that("prefixList() can filter by a subset of prefixes", {
  skip_on_cran()
  skip_if_offline()
  
  # Get full list first
  all_prefixes_matrix <- prefixList("CELLAR")
  
  # Extract dynamic prefixes from classificationEndpoint() indirectly:
  # keep only rows that match "PREFIX XXX:" where XXX is uppercase letters/numbers
  prefix_names <- sub("^PREFIX\\s+([^:]+):.*$", "\\1", all_prefixes_matrix[, 1])
  
  # Remove the hard-coded base prefixes by excluding the most common ones
  base_prefixes <- c("dc", "dct", "cb", "eli", "euvoc", "owl", "rdfs",
                     "skosxl", "xml", "rdf", "skos", "xkos", "is",
                     "isi", "cpc", "xsd")
  
  dynamic_prefixes <- setdiff(prefix_names, base_prefixes)
  
  skip_if(length(dynamic_prefixes) == 0L,
          message = "No dynamic prefixes found for CELLAR endpoint; cannot test filtering.")
  
  # Take one dynamic prefix
  target_prefix <- dynamic_prefixes[1L]
  
  filtered <- prefixList("CELLAR", prefix = target_prefix)
  
  expect_true(is.matrix(filtered))
  expect_true(is.character(filtered))
  
  # Check that the requested prefix is present
  filtered_prefix_names <- sub("^PREFIX\\s+([^:]+):.*$", "\\1", filtered[, 1])
  expect_true(target_prefix %in% filtered_prefix_names)
  
  # And at least one base prefix should still be present
  expect_true(any(base_prefixes %in% filtered_prefix_names))
})

test_that("prefixList() errors when requested prefixes do not exist", {
  skip_on_cran()
  skip_if_offline()
  
  expect_error(
    prefixList("CELLAR", prefix = "THISDOESNOTEXIST"),
    regexp = "Desired prefixes not found for endpoint"
  )
})
