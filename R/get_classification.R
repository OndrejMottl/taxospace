#' Get classification for a taxonomic name
#'
#' This function takes a taxonomic name and returns its classification.
#' It retrieves the classification from the Global Biodiversity Information
#' Facility (GBIF) and Integrated Taxonomic Information System (ITIS)
#' databases. If the name is not found in either database,
#' an empty list is returned.
#'
#' @param taxon A character with a taxonomic name
#' @param sel_db_name A string indicating which database to use to
#' resolve names. Default is "gnr".
#' @param interactive A logical value indicating whether to ask the user for
#' input or automaticaly pick the best match found in the database
#' @param verbose Logical. If TRUE the additional messages are printed on
#' the console
#' @return A list with the following elements:
#' \item{sel_name}{The selected taxonomic name}
#' \item{data_resolve}{A data.frame with information about the resolved name,
#' including the matched name, data source title, and match score}
#' \item{db}{The database from which the classification was retrieved
#' (GBIF or ITIS)}
#' \item{id}{The most matching ID of the taxonomic name in the database}
#' \item{classification}{A data.frame with the classification of the taxonomic
#' name, including the name, rank, and ID}
#' @export
#' @examples
#' get_classification("Homo sapiens")
#' get_classification(c("Canis lupus", "Felis catus"))
#' get_classification("Pikachu")
#' @seealso
#' [get_classification_buk], [taxize::resolve], [taxize::classification]
get_classification <- function(
    taxon,
    sel_db_name = c("gnr", "iplant"),
    interactive = TRUE,
    verbose = FALSE) {
  # prealocate space
  list_sel_taxon <-
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

  sel_db_name <- match.arg(sel_db_name)

  # add taxon name
  list_sel_taxon$sel_name <-
    taxon

  # resolve taxon
  data_taxon_resolve <-
    taxize::resolve(
      sci = list_sel_taxon$sel_name,
      db = sel_db_name
    ) %>%
    purrr::pluck(1)

  if (
    all(is.na(data_taxon_resolve))
  ) {
    return(list_sel_taxon)
  }

  # save the best match
  list_sel_taxon$data_resolve <-
    data_taxon_resolve %>%
    dplyr::filter(
      get("score") == max(get("score"))
    )

  if (
    isTRUE(interactive)
  ) {
    # get id (GBIF)
    suppressWarnings(
      taxon_mached_name_id_check <-
        taxize::get_gbifid(
          sci = list_sel_taxon$data_resolve$matched_name,
          messages = verbose,
          ask = TRUE
        )
    )

    # If there is nothing in GBIF, try ITIS
    if (
      all(is.na(taxon_mached_name_id_check))
    ) {
      list_sel_taxon$db <- "itis"

      suppressWarnings(
        taxon_mached_name_id_check <-
          taxize::get_tsn(
            sci = list_sel_taxon$data_resolve$matched_name,
            messages = verbose,
            ask = TRUE,
            accepted = FALSE
          )
      )
    }

    # If there is nothing, return empty
    if (
      all(is.na(taxon_mached_name_id_check))
    ) {
      base::message("data does not find")

      return(list_sel_taxon)
    }

    data_taxon_mached_name_id <-
      data.frame(
        matched_name = list_sel_taxon$data_resolve$matched_name,
        id = as.character(taxon_mached_name_id_check)
      )
  } else {
    # get id (GBIF)
    suppressWarnings(
      taxon_mached_name_id_check <-
        taxize::get_gbifid_(
          sci = list_sel_taxon$data_resolve$matched_name,
          messages = verbose
        )
    )

    # If there is nothing in GBIF, try ITIS
    if (
      purrr::map_lgl(
        .x = taxon_mached_name_id_check,
        .f = ~ nrow(.x) == 0
      ) %>%
        all()
    ) {
      list_sel_taxon$db <- "itis"

      suppressWarnings(
        taxon_mached_name_id_check <-
          taxize::get_tsn_(
            sci = list_sel_taxon$data_resolve$matched_name,
            messages = verbose,
            accepted = FALSE
          )
      )
    }

    # If there is nothing, return empty
    if (
      purrr::map_lgl(
        .x = taxon_mached_name_id_check,
        .f = ~ nrow(.x) == 0
      ) %>%
        all()
    ) {
      base::message("data does not find")

      return(list_sel_taxon)
    }

    # turn it into a data frame
    data_taxon_mached_name_id_full <-
      taxon_mached_name_id_check %>%
      purrr::map(
        .f = ~ {
          if (
            "ACCEPTED" %in% .x$status
          ) {
            dplyr::filter(.x, status == "ACCEPTED") %>%
              dplyr::slice(1)
          } else {
            dplyr::slice(.x, 1)
          }
        }
      ) %>%
      dplyr::bind_rows(
        .id = "matched_name"
      )

    data_taxon_mached_name_id <-
      data_taxon_mached_name_id_full %>%
      dplyr::select(
        matched_name,
        id = usagekey
      )
  }

  # save the most matching ID
  list_sel_taxon$id <-
    data_taxon_mached_name_id %>%
    dplyr::group_by(id) %>%
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
    dplyr::filter(
      get("Freq") == max(get("Freq"))
    ) %>%
    dplyr::mutate(
      "id" = as.character(get("id"))
    ) %>%
    dplyr::ungroup() %>%
    purrr::pluck(1, 1) %>%
    as.character()

  # save classification
  list_sel_taxon$classification <-
    taxize::classification(
      sci_id = list_sel_taxon$id,
      db = list_sel_taxon$db
    ) %>%
    purrr::pluck(1) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    )


  return(list_sel_taxon)
}
