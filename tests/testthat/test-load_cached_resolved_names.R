testthat::test_that("load_cached_resolved_names loads data correctly", {
  vec_file_names <- c("file1", "file2", "file3")

  # Create some dummy data
  data_dummy <-
    data.frame(
      usagekey = 1:3,
      scientificname = c("name1", "name2", "name3"),
      rank = c("rank1", "rank2", "rank3"),
      status = c("status1", "status2", "status3"),
      matchtype = c("matchtype1", "matchtype2", "matchtype3"),
      canonicalname = c("canonicalname1", "canonicalname2", "canonicalname3"),
      confidence = 1:3,
      kingdom = c("kingdom1", "kingdom2", "kingdom3"),
      phylum = c("phylum1", "phylum2", "phylum3"),
      order = c("order1", "order2", "order3"),
      family = c("family1", "family2", "family3"),
      genus = c("genus1", "genus2", "genus3"),
      species = c("species1", "species2", "species3"),
      kingdomkey = 1:3,
      phylumkey = 1:3,
      classkey = 1:3,
      orderkey = 1:3,
      familykey = 1:3,
      genuskey = 1:3,
      specieskey = 1:3,
      synonym = c(TRUE, FALSE, TRUE),
      class = c("class1", "class2", "class3"),
      note = c("note1", "note2", "note3"),
      acceptedusagekey = 1:3
    )

  dummy_list <-
    list(data_dummy, data_dummy, data_dummy) %>%
    rlang::set_names(vec_file_names)

  cache_dataframe(dummy_list, sel_dir = "resolve")

  # Call the function under test
  result <-
    load_cached_resolved_names(vec_file_names)

  # Check if the result is a data frame
  testthat::expect_true(is.data.frame(result))

  # Check if the column names are correct
  expected_cols <- c(
    "name_clean",
    "usagekey", "scientificname", "rank", "status", "matchtype",
    "canonicalname",
    "confidence", "kingdom", "phylum", "order", "family", "genus", "species",
    "kingdomkey", "phylumkey", "classkey", "orderkey", "familykey", "genuskey",
    "specieskey", "synonym", "class", "note", "acceptedusagekey"
  )

  testthat::expect_equal(colnames(result), expected_cols)

  # Clean up the temporary directory
  unlink(paste0(tempdir(), "/resolve"), recursive = TRUE, force = TRUE)
})
