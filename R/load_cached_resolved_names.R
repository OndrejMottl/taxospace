#' @title Load cached resolved names
#' @description
#' This function takes a data vector as input and loads the cached resolved
#' names.
#' @param data_vector
#' A vector of file names to load the cached resolved names from.
#' @return
#' A data frame containing the loaded data.
load_cached_resolved_names <- function(data_vector) {
  data_vector %>%
    purrr::set_names() %>%
    purrr::map(
      .progress = "loading data from cache",
      .f = ~ readr::read_tsv(
        file = paste0(
          tempdir(),
          "/taxospace/",
          .x,
          ".txt"
        ),
        col_types = readr::cols(
          usagekey = readr::col_integer(),
          scientificname = readr::col_character(),
          rank = readr::col_character(),
          status = readr::col_character(),
          matchtype = readr::col_character(),
          canonicalname = readr::col_character(),
          confidence = readr::col_integer(),
          kingdom = readr::col_character(),
          phylum = readr::col_character(),
          order = readr::col_character(),
          family = readr::col_character(),
          genus = readr::col_character(),
          species = readr::col_character(),
          kingdomkey = readr::col_integer(),
          phylumkey = readr::col_integer(),
          classkey = readr::col_integer(),
          orderkey = readr::col_integer(),
          familykey = readr::col_integer(),
          genuskey = readr::col_integer(),
          specieskey = readr::col_integer(),
          synonym = readr::col_logical(),
          class = readr::col_character(),
          note = readr::col_character(),
          acceptedusagekey = readr::col_integer()
        ), show_col_types = FALSE
      )
    ) %>%
    dplyr::bind_rows(.id = "name_clean")
}
