#' @title Get the names of cached files in the taxospace directory.
#' @description
#' This function retrieves the names of cached files in the taxospace directory.
#' @return A character vector containing the names of the cached files.
get_cached_file_names <- function(sel_dir) {
  list.files(
    paste0(
      tempdir(), "/taxospace/", sel_dir, "/"
    ),
    pattern = ".txt"
  ) %>%
    stringr::str_remove(".txt")
}
