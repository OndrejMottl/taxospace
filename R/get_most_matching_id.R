#' @title Get the most matching ID
#' @description
#' This function takes two data frames as input:
#' \code{data_with_nested_resolve} and \code{data_taxa_mached_name_id}.
#' It performs a series of operations to find the most matching ID based on
#' the frequncy of the matched names.
#' @param data_with_nested_resolve
#' A data frame containing the nested column \code{data_resolve}.
#' @param data_taxa_mached_name_id
#' A data frame containing the column \code{matched_name}.
#' @return A modified data frame with the most matching ID.
#' @importFrom rlang .data
get_most_matching_id <- function(
    data_with_nested_resolve, data_taxa_mached_name_id) {
  assertthat::assert_that(
    is.data.frame(data_with_nested_resolve),
    msg = "data_with_nested_resolve must be a data frame"
  )

  assertthat::assert_that(
    assertthat::has_name(data_with_nested_resolve, "data_resolve"),
    msg = "data_with_nested_resolve must have a column named data_resolve"
  )

  assertthat::assert_that(
    is.data.frame(data_taxa_mached_name_id),
    msg = "data_taxa_mached_name_id must be a data frame"
  )

  assertthat::assert_that(
    assertthat::has_name(data_taxa_mached_name_id, "matched_name"),
    msg = "data_taxa_mached_name_id must have a column named matched_name"
  )

  data_with_nested_resolve %>%
    tidyr::unnest("data_resolve") %>%
    dplyr::select("sel_name", "matched_name") %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      data_taxa_mached_name_id,
      by = "matched_name"
    ) %>%
    dplyr::group_by(
      dplyr::across(
        c("sel_name", "id")
      )
    ) %>%
    dplyr::summarise(
      .groups = "drop",
      Freq = dplyr::n()
    ) %>%
    tidyr::drop_na() %>%
    dplyr::arrange(
      dplyr::desc(
        dplyr::pick("Freq")
      )
    ) %>%
    dplyr::group_by(
      dplyr::across("sel_name")
    ) %>%
    dplyr::filter(
      get("Freq") == max(get("Freq"))
    ) %>%
    dplyr::mutate(
      "id" = as.character(get("id"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(!dplyr::any_of("Freq")) %>%
    return()
}
