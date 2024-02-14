testthat::test_that(
  "cache_dataframe correctly caches dataframes",
  {
    # Create some sample dataframes
    df1 <- data.frame(a = 1:3, b = 4:6)
    df2 <- data.frame(x = letters[1:3], y = LETTERS[1:3])

    # Create a named list of dataframes
    data_vec <- list(df1 = df1, df2 = df2)

    # Specify the directory to save the dataframes
    sel_dir <- "cache_test"

    # Call the cache_dataframe function
    cache_dataframe(data_vec, sel_dir)

    # Check if the files were created
    testthat::expect_true(
      file.exists(
        paste0(tempdir(), "/taxospace/", sel_dir, "/df1.txt")
      )
    )
    testthat::expect_true(
      file.exists(
        paste0(tempdir(), "/taxospace/", sel_dir, "/df2.txt")
      )
    )
  }
)

testthat::test_that(
  "cache_dataframe throws an error for invalid input",
  {
    # Create an invalid input (not a named list)
    data_vec <- c(1, 2, 3)

    # Specify the directory to save the dataframes
    sel_dir <- "cache_test"

    # Call the cache_dataframe function and expect an error
    testthat::expect_error(
      cache_dataframe(data_vec, sel_dir),
      "data_vec must be a list."
    )

    # Create an invalid input (empty list)
    data_vec <- list()

    # Call the cache_dataframe function and expect an error
    testthat::expect_error(
      cache_dataframe(data_vec, sel_dir),
      "data_vec must have at least one element."
    )

    # Create an invalid input (not all elements are data frames)
    data_vec <-
      list(
        df1 = data.frame(a = 1:3, b = 4:6),
        df2 = "not a dataframe"
      )

    # Call the cache_dataframe function and expect an error
    testthat::expect_error(
      cache_dataframe(data_vec, sel_dir),
      "All elements of data_vec must be data frames."
    )
  }
)
