#' @title Selects the best resolve
#' @description
#' Filters the rows based on the maximum score for for each group.
#' @param data_resolve The input data frame containing the resolves.
#' @return A data frame with the selected resolves.
select_best_resolve <- function(data_resolve) {
  data_resolve %>%
    dplyr::group_by(
      dplyr::across("user_supplied_name")
    ) %>%
    dplyr::filter(
      get("score") == max(get("score"))
    ) %>%
    dplyr::ungroup()
}
