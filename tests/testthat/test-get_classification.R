testthat::test_that("get_classification returns the correct classification", {
  # Test case 1: Test with a full species name
  result1 <- get_classification("Homo sapiens", interactive = TRUE)
  testthat::expect_equal(result1$db, "gbif")
  testthat::expect_equal(result1$id, "2436436")
  testthat::expect_true(nrow(result1$classification) > 0)
  testthat::expect_equal(nrow(result1$classification), 7)

  # Test case 2: Test with a partial species name
  result2 <- get_classification("acacia_type", interactive = FALSE)
  testthat::expect_equal(result2$db, "gbif")
  testthat::expect_equal(result2$id, "2978223")
  testthat::expect_true(nrow(result2$classification) > 0)
  testthat::expect_equal(nrow(result2$classification), 6)

  # Test case 3: Test with a partial species name
  result3 <- get_classification("Felis Catus", interactive = FALSE)
  testthat::expect_equal(result3$db, "gbif")
  testthat::expect_equal(result3$id, "2435022")
  testthat::expect_true(nrow(result3$classification) > 0)
  testthat::expect_equal(nrow(result3$classification), 6)

  # Test case 4: Test with a nonexistent taxonomic name
  result4 <- get_classification("Pikachu", interactive = FALSE)
  testthat::expect_true(nrow(result4$data_resolve) == 0)
  testthat::expect_true(nrow(result4$classification) == 0)
  testthat::expect_identical(result4$id, NA_character_)
})
