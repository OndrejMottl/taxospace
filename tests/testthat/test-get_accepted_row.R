# Test when 'status' column exists
testthat::test_that("returns the first row with status 'ACCEPTED'", {
  # Create a sample data frame
  data_test <-
    data.frame(
      status = c("REJECTED", "ACCEPTED", "ACCEPTED", "PENDING"),
      confidence = c(0.9, 0.7, 0.8, 0.7),
      value = c(1, 2, 3, 4)
    )

  result <- get_accepted_row(data_test)

  expected_row <-
    data.frame(
      status = "ACCEPTED",
      confidence = 0.8,
      value = 3
    )

  testthat::expect_equal(result, expected_row)
})

# Test when 'status' column does not exist
testthat::test_that("returns the first row without status", {
  # Create a sample data frame
  data_without_status <-
    data.frame(
      value = c(1, 2, 3)
    )

  result <- get_accepted_row(data_without_status)

  expected_row <-
    data.frame(
      value = 1
    )

  testthat::expect_equal(result, expected_row)
})

# test when status is present but no accepted rows
testthat::test_that("returns the first row without status", {
  # Create a sample data frame
  data_test <-
    data.frame(
      status = c("REJECTED", "ACCEPTED", "ACCEPTED", "PENDING"),
      confidence = c(0.9, 0.7, 0.8, 0.7),
      value = c(1, 2, 3, 4)
    )

  result <- get_accepted_row(data_test, only_accepted = FALSE)

  expected_row <-
    data.frame(
      status = "REJECTED",
      confidence = 0.9,
      value = 1
    )

  testthat::expect_equal(result, expected_row)
})
