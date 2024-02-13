# Test that function returns correct classification for Homo sapiens
testthat::test_that(" returns correct classification for Homo sapiens", {
  result_homo <- get_classification_buk(c("Homo sapiens"), use_cache = FALSE)
  testthat::expect_equal(nrow(result_homo$classification[[1]]), 7)
  testthat::expect_equal(
    result_homo$classification[[1]]$name[7], "Homo sapiens"
  )
})

# Test that function returns correct classification for Betula
testthat::test_that(" returns correct classification for Betula", {
  result_betula <- get_classification_buk(c("Betula"), use_cache = FALSE)
  testthat::expect_equal(nrow(result_betula$classification[[1]]), 6)
  testthat::expect_equal(result_betula$classification[[1]]$name[6], "Betula")
})

# Test that function returns NA if taxon is not found
testthat::test_that(" returns no match if taxon is not found", {
  result <- get_classification_buk(c("Pikachu"), use_cache = FALSE)
  testthat::expect_true(colnames(result) == "sel_name")
  testthat::expect_equal(nrow(result), 1)
})

result_both <- get_classification_buk(c("Homo sapiens", "Betula"), use_cache = FALSE)

# Test that function returns a data frame
testthat::test_that(" returns a data frame", {
  testthat::expect_s3_class(result_both, "data.frame")
})

# Test that function returns correct columns
testthat::test_that(" returns correct columns", {
  testthat::expect_true(
    all(
      c("sel_name", "id", "data_resolve", "classification") %in%
        colnames(result_both)
    )
  )
})

# Test that function returns correct number of rows
testthat::test_that(" returns correct number of rows", {
  testthat::expect_equal(nrow(result_both), 2)
})
