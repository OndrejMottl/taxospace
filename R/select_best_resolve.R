#' @title Selects the best resolve
#' @description
#' Filters the rows based on the maximum score for for each group.
#' @param data_resolve The input data frame containing the resolves.
#' @return A data frame with the selected resolves.
select_best_resolve <- function(data_resolve) {
  data_resolve %>%
    dplyr::group_by(
      dplyr::across("submittedName")
    ) %>%
    dplyr::filter(
      get("sortScore") == max(get("sortScore"))
    ) %>%
    dplyr::ungroup()
}
