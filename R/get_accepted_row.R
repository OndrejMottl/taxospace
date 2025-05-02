#' @title Get the first row with status "ACCEPTED" from a data source
#' @description
#' This function filters the data source to retrieve the first row
#' with the status "ACCEPTED".
#' If the data source does not have a "status" column, it returns the first row.
#' @param data_source The data source to filter
#' @param only_accepted
#' Logical indicating whether to filter for "ACCEPTED" status
#' @return
#' The first row with status "ACCEPTED" or the first row if "status" column
#' is not present
#' @importFrom rlang .data
get_accepted_row <- function(data_source, only_accepted = TRUE) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source must be a data frame."
  )

  assertthat::assert_that(
    nrow(data_source) > 0,
    msg = "data_source must have at least one row."
  )

  assertthat::assert_that(
    is.logical(only_accepted),
    msg = "only_accepted must be a logical value."
  )

  assertthat::assert_that(
    length(only_accepted) == 1,
    msg = "only_accepted must be a single logical value."
  )

  res <- data_source

  if (
    !"status" %in% names(res)
  ) {
    res <-
      res %>%
      dplyr::slice(1)

    return(res)
  }

  if (
    "ACCEPTED" %in% unique(data_source$status) && isTRUE(only_accepted)
  ) {
    res <-
      res %>%
      dplyr::filter(.data$status == "ACCEPTED")
  }

  res <-
    res %>%
    dplyr::filter(.data$confidence == max(.data$confidence)) %>%
    dplyr::slice(1)

  return(res)
}
