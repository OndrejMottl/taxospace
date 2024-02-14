#' @title Get the first row with status "ACCEPTED" from a data source
#' @description
#' This function filters the data source to retrieve the first row
#' with the status "ACCEPTED".
#' If the data source does not have a "status" column, it returns the first row.
#' @param data_source The data source to filter
#' @return
#' The first row with status "ACCEPTED" or the first row if "status" column
#' is not present
#' @importFrom rlang .data
get_accepted_row <- function(data_source) {
  res <-
    data_source %>%
    dplyr::slice(1)

  if (
    "status" %in% names(data_source)
  ) {
    if (
      "ACCEPTED" %in% unique(data_source$status)
    ) {
      res <-
        data_source %>%
        dplyr::filter(.data$status == "ACCEPTED") %>%
        dplyr::slice(1)
    }
  }

  return(res)
}
