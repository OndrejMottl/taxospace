#' Get classification for a taxonomic name
#'
#' This function takes a taxonomic name and returns its classification.
#' It retrieves the classification from the Global Biodiversity Information
#' Facility (GBIF) and Integrated Taxonomic Information System (ITIS)
#' databases. If the name is not found in either database,
#' an empty list is returned.
#'
#' @param taxa A character vector of taxonomic names
#' @param interactive A logical value indicating whether to ask the user for
#' input when there is no match found in the GBIF database
#'
#' @return A list with the following elements:
#' \item{sel_name}{The selected taxonomic name}
#' \item{data_resolve}{A data.frame with information about the resolved name,
#' including the matched name, data source title, and match score}
#' \item{db}{The database from which the classification was retrieved
#' (GBIF or ITIS)}
#' \item{id}{The most matching ID of the taxonomic name in the database}
#' \item{classification}{A data.frame with the classification of the taxonomic
#' name, including the name, rank, and ID}
#' @examples
#' get_classification("Homo sapiens")
#' get_classification(c("Canis lupus", "Felis catus"))
#' get_classification("Pikachu")
#'
#' @export
get_classification <- function(taxa, interactive = TRUE) {
  # prealocate space
  list_sel_taxa <-
    list(
      sel_name = NA_character_,
      data_resolve = data.frame(
        user_supplied_name = NA_character_,
        submitted_name = NA_character_,
        matched_name = NA_character_,
        data_source_title = NA_character_,
        score = NA_real_
      ) %>%
        tidyr::drop_na(),
      db = "gbif",
      id = NA_character_,
      classification = data.frame(
        name = NA_character_,
        rank = NA_character_,
        id = NA_character_
      ) %>%
        tidyr::drop_na()
    )

  # add taxa mname
  list_sel_taxa$sel_name <-
    taxa

  # resolve taxa
  data_taxon_resolve <-
    taxize::resolve(list_sel_taxa$sel_name) %>%
    purrr::pluck(1)

  if (
    all(is.na(data_taxon_resolve))
  ) {
    return(list_sel_taxa)
  }

  # save the best match
  list_sel_taxa$data_resolve <-
    data_taxon_resolve %>%
    dplyr::filter(
      get("score") == max(get("score"))
    )

  # get id (GBIF)
  suppressWarnings(
    taxa_mached_name_id_check <-
      taxize::get_gbifid(
        sci = list_sel_taxa$data_resolve$matched_name,
        messages = FALSE,
        ask = interactive
      )
  )

  # If there is nothing in GBIF, try ITIS
  if (
    all(is.na(taxa_mached_name_id_check))
  ) {
    list_sel_taxa$db <- "itis"

    suppressWarnings(taxa_mached_name_id_check <-
      taxize::get_tsn(
        sci = list_sel_taxa$data_resolve$matched_name,
        messages = FALSE,
        ask = interactive,
        accepted = FALSE
      ))
  }

  # If there is nothing in ITIS, try return empty
  if (
    all(is.na(taxa_mached_name_id_check))
  ) {
    base::message("data does not find")

    return(list_sel_taxa)
  }

  # save the most matching ID
  list_sel_taxa$id <-
    taxa_mached_name_id_check %>%
    table() %>%
    as.data.frame() %>%
    dplyr::arrange(
      dplyr::desc(
        dplyr::pick("Freq")
      )
    ) %>%
    purrr::pluck(1, 1) %>%
    as.character()

  # save classification
  list_sel_taxa$classification <-
    taxize::classification(
      sci_id = list_sel_taxa$id,
      db = list_sel_taxa$db
    ) %>%
    purrr::pluck(1) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    )


  return(list_sel_taxa)
}
