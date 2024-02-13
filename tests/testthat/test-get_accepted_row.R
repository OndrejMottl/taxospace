# Test when 'status' column exists
testthat::test_that("returns the first row with status 'ACCEPTED'", {
  # Create a sample data frame
  data <-
    data.frame(
      status = c("REJECTED", "ACCEPTED", "PENDING"),
      value = c(1, 2, 3)
    )
  result <- get_accepted_row(data)

  testthat::expect_equal(result, data.frame(status = "ACCEPTED", value = 2))
})

# Test when 'status' column does not exist
testthat::test_that("returns the first row without status", {
  # Create a sample data frame
  data_without_status <-
    data.frame(
      value = c(1, 2, 3)
    )
  result <- get_accepted_row(data_without_status)

  testthat::expect_equal(result, data.frame(value = 1))
})
