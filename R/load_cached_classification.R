#' @title Load cached classification
#' @description
#' This function takes a data vector as input and loads the cached 
#' classification
#' @param data_vector
#' A vector of file names to load the cached classification names from.
#' @return
#' A data frame containing the loaded data.
load_cached_classification <- function(data_vector) {
  data_vector %>%
    purrr::set_names() %>%
    purrr::map(
      .progress = "loading data from cache",
      .f = ~ readr::read_tsv(
        file = paste0(
          tempdir(),
          "/taxospace/classification/",
          .x,
          ".txt"
        ),
        col_types = readr::cols(
          name = readr::col_character(),
          rank = readr::col_character(),
          id = readr::col_integer()
        ), show_col_types = FALSE
      )
    ) %>%
    return()
}
