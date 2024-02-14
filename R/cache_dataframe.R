#' @title Cache datafra,e
#' @description
#' Takes a named list with dataframes and caches the results by writing
#' each dataframe into a separate text file.
#' @param data_vec A named list containing the data.frames.
#' @param sel_dir A character string specifying the directory to save the
#' dataframes.
#' @param clean_name A logical value specifying whether to clean the names of
#' the dataframes before saving them. Default is `FALSE`.
#' @return NULL
cache_dataframe <- function(data_vec, sel_dir, clean_name = FALSE) {
  assertthat::assert_that(
    is.list(data_vec),
    msg = "data_vec must be a list."
  )

  assertthat::assert_that(
    length(data_vec) > 0,
    msg = "data_vec must have at least one element."
  )

  assertthat::assert_that(
    names(data_vec) %>% length() > 0,
    msg = "data_vec must be named list"
  )

  assertthat::assert_that(
    all(sapply(data_vec, is.data.frame)),
    msg = "All elements of data_vec must be data frames."
  )

  dir.create(
    paste0(
      tempdir(), "/taxospace/", sel_dir, "/"
    ),
    recursive = TRUE,
    showWarnings = FALSE
  )

  purrr::iwalk(
    .x = data_vec,
    .f = ~ readr::write_tsv(
      x = .x,
      file = paste0(
        tempdir(),
        "/taxospace/",
        sel_dir,
        "/",
        ifelse(clean_name, janitor::make_clean_names(.y), .y),
        ".txt"
      )
    )
  )
}
