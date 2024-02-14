testthat::test_that("get_cached_file_names returns correct file names", {
  # Create temporary directory for testing
  test_dir <- tempdir()

  dir.create(
    file.path(test_dir, "taxospace", "dir1"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(test_dir, "taxospace", "dir2"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Create some test files
  file.create(
    file.path(test_dir, "taxospace", "dir1", "file1.txt"),
    showWarnings = FALSE
  )
  file.create(
    file.path(test_dir, "taxospace", "dir1", "file2.txt"),
    showWarnings = FALSE
  )
  file.create(
    file.path(test_dir, "taxospace", "dir2", "file3.txt"),
    showWarnings = FALSE
  )

  # Test with different directories
  testthat::expect_equal(
    get_cached_file_names("dir1"),
    c("file1", "file2")
  )
  testthat::expect_equal(
    get_cached_file_names("dir2"),
    c("file3")
  )

  # Clean up
  unlink(
    file.path(test_dir, "taxospace"),
    recursive = TRUE,
    force = TRUE
  )
})

testthat::test_that("get_cached_file_names returns empty vector when no files found", {
  # Create temporary directory for testing
  test_dir <- tempdir()

  dir.create(
    file.path(test_dir, "taxospace", "dirXYZ"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Test with empty directory
  testthat::expect_equal(
    get_cached_file_names("dirXYZ"), character(0)
  )
  # Clean up
  unlink(
    file.path(test_dir, "taxospace"),
    recursive = TRUE,
    force = TRUE
  )
})
