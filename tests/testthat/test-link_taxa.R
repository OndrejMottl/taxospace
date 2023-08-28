example1 <- link_taxa(
  taxon_a = "Homo sapiens",
  taxon_b = "Homo denisova"
)

# test that link_taxa returns a data frame
testthat::test_that("link_taxa returns a data frame", {
  testthat::expect_true(is.data.frame(example1$common_classification))
})

# test that link_taxa returns correct number of rows
testthat::test_that("link_taxa returns correct number of rows", {
  testthat::expect_equal(nrow(example1$common_classification), 6)
})

# test that link_taxa return correct common class
testthat::test_that("link_taxa return correct common class", {
  testthat::expect_equal(example1$highest_common_level, "genus")
  testthat::expect_equal(example1$highest_common_level_name, "Homo")
  testthat::expect_equal(example1$highest_common_level_id, "2436435")
})

# test that link_taxa only works if both taxa can be classified
testthat::test_that("link_taxa only works if both taxa can be classified", {
  testthat::expect_error(
    link_taxa(
      taxon_a = "Pikachu",
      taxon_b = "Homo sapiens"
    )
  )

  testthat::expect_error(
    link_taxa(
      taxon_b = "Pikachu",
      taxon_a = "Homo sapiens"
    )
  )
})
