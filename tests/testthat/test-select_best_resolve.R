testthat::test_that("select_best_resolve returns the expected output", {
  # Create sample input data
  data_resolve <-
    data.frame(
      submittedName = c("A", "A", "B", "B", "C", "C"),
      sortScore = c(10, 20, 30, 40, 50, 60)
    )

  # Expected output
  expected_output <-
    tibble::tibble(
      submittedName = c("A", "B", "C"),
      sortScore = c(20, 40, 60)
    )

  # Call the function
  result <-
    select_best_resolve(data_resolve)

  # Check if the output matches the expected output
  testthat::expect_equal(result, expected_output)
})
