#' @title Get common classification for two taxons
#' @description Get the individuial classification for taxons and then find
#' the most releated common classification level.
#' @param taxon_a A character with a taxonomic name
#' @param taxon_b A character with a taxonomic name
#' @param ... Additional arguments pased to [get_classification]
#' @return A list with the following elements:
#' \item{taxon_a}{A data frame with the classification of taxon_a}
#' \item{taxon_b}{A data frame with the classification of taxon_b}
#' \item{common_classification}{A data frame with the common classification}
#' \item{highest_common_level}{The highest common classification level}
#' \item{highest_common_level_name}{The name of the highest common classification level}
#' \item{highest_common_level_id}{The ID of the highest common classification level}
#' @seealso [get_classification]
#' @examples
#' link_taxa(taxon_a = "Homo sapiens", taxon_b = "Homo denisova")
#' link_taxa("Canis lupus", "Felis catus")
#' @export
#' @importFrom rlang .data
link_taxa <- function(
    taxon_a,
    taxon_b,
    ...) {
  class_taxa_a <- get_classification(taxon_a, ...)

  try(
    class_taxa_a_extract <-
      class_taxa_a %>%
      dplyr::filter(
        .data$sel_name == taxon_a
      ) %>%
      purrr::pluck("classification", 1),
    silent = TRUE
  )

  if (
    nrow(class_taxa_a_extract) < 1
  ) {
    stop("No classification found for taxon_a")
  }

  class_taxa_b <- get_classification(taxon_b, ...)

  try(
    class_taxa_b_extract <-
      class_taxa_b %>%
      dplyr::filter(
        .data$sel_name == taxon_b
      ) %>%
      purrr::pluck("classification", 1),
    silent = TRUE
  )

  if (
    nrow(class_taxa_b_extract) < 1
  ) {
    stop("No classification found for taxon_b")
  }

  data_common_classification <-
    dplyr::inner_join(
      class_taxa_a_extract,
      class_taxa_b_extract,
      by = c("name", "rank", "id")
    )

  data_highest_common_classification <-
    data_common_classification %>%
    dplyr::slice_tail(n = 1)

  return(
    list(
      taxon_a = class_taxa_a_extract,
      taxon_b = class_taxa_b_extract,
      common_classification = data_common_classification,
      highest_common_level = data_highest_common_classification %>%
        purrr::chuck("rank"),
      highest_common_level_name = data_highest_common_classification %>%
        purrr::chuck("name"),
      highest_common_level_id = data_highest_common_classification %>%
        purrr::chuck("id")
    )
  )
}
