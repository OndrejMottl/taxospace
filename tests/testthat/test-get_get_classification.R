testthat::test_that("get_classification returns the correct classification", {
  # Test case 1: Test with a full species name
  result1 <- get_classification("Homo sapiens", interactive = FALSE)
  testthat::expect_equal(result1$db, "gbif")
  testthat::expect_equal(result1$id, "2436436")
  testthat::expect_true(nrow(result1$classification) > 0)
  testthat::expect_equal(nrow(result1$classification), 7)


  # Test case 2: Test with a partial species name
  result2 <- get_classification("acer_type", interactive = FALSE)
  testthat::expect_equal(result2$db, "gbif")
  testthat::expect_equal(result2$id, "3189834")
  testthat::expect_true(nrow(result2$classification) > 0)
  testthat::expect_equal(nrow(result2$classification), 6)

  # Test case 3: Test with a nonexistent taxonomic name
  result3 <- get_classification("Pikachu", interactive = FALSE)
  testthat::expect_true(nrow(result3$data_resolve) == 0)
  testthat::expect_true(nrow(result3$classification) == 0)
  testthat::expect_identical(result3$id, NA_character_)
})
