# Test that function returns correct classification for Homo sapiens
testthat::test_that(" returns correct classification for Homo sapiens", {
  result_homo <- get_classification(c("Homo sapiens"))
  testthat::expect_equal(nrow(result_homo$classification[[1]]), 7)
  testthat::expect_equal(
    result_homo$classification[[1]]$name[7], "Homo sapiens"
  )
})

# Test that function returns correct classification for Betula
testthat::test_that(" returns correct classification for Betula", {
  result_betula <- get_classification(c("Betula"))
  testthat::expect_equal(nrow(result_betula$classification[[1]]), 6)
  testthat::expect_equal(result_betula$classification[[1]]$name[6], "Betula")
})

# Test that function returns NA if taxon is not found
testthat::test_that(" returns no match if taxon is not found", {
  result <- get_classification(c("Pikachu"))
  testthat::expect_true(colnames(result) == "sel_name")
  testthat::expect_equal(nrow(result), 1)
})

result_both <- get_classification(c("Homo sapiens", "Betula"))

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

# Test that function uses cache when use_cache = TRUE
testthat::test_that("uses cache when use_cache = TRUE", {
  result_cache <-
    get_classification(c("Homo sapiens"), use_cache = TRUE)

  cached_resolved <-
    get_cached_file_names(sel_dir = "resolve")

  cached_resolved_expected <- "homo_sapiens_linnaeus_1758"


  testthat::expect_contains(
    cached_resolved, cached_resolved_expected
  )

  cached_clasification <-
    get_cached_file_names(sel_dir = "classification")

  testthat::expect_equal(
    cached_clasification, "2436436"
  )

  result_cache_again <-
    get_classification(c("Homo sapiens"), use_cache = TRUE)

  testthat::expect_identical(result_cache, result_cache_again)

  # Clean up
  unlink(
    file.path(tempdir(), "taxospace"),
    recursive = TRUE,
    force = TRUE
  )
})

# Test that function returns exit early in scenario when there is no ID
testthat::test_that(" returns correct number of rows", {
  result_angiospermae <-
    get_classification("Angiospermae")

  testthat::expect_true(
    is.data.frame(result_angiospermae) &&
      ncol(result_angiospermae) == 2
  )
})
