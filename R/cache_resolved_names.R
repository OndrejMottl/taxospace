#' @title Cache resolved names
#' @description
#' Takes a named list with resolved names and caches the results by writing
#' each unique resolved name to a separate text file.
#' @param data_vec A named list containing the resolved names (data.frames).
#' @return NULL
cache_resolved_names <- function(data_vec) {
  assertthat::assert_that(
    is.list(data_vec),
    msg = "data_vec must be a list."
  )

  assertthat::assert_that(
    names(data_vec) %>% length() > 0,
    msg = "data_vec must be named list"
  )

  assertthat::assert_that(
    length(data_vec) > 0,
    msg = "data_vec must have at least one element."
  )

  assertthat::assert_that(
    all(sapply(data_vec, is.data.frame)),
    msg = "All elements of data_vec must be data frames."
  )

  dir.create(file.path(tempdir(), "taxospace/resolved/"), showWarnings = FALSE)

  data_vec %>%
    purrr::iwalk(
      .f = ~ readr::write_tsv(
        x = .x,
        file = paste0(
          tempdir(),
          "/taxospace/resolved/",
          janitor::make_clean_names(.y),
          ".txt"
        )
      )
    )
}
