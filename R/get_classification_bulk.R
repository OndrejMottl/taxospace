#' @title Get classification for given vector of taxa
#' @description
#' This function takes a vector of taxa names and returns the classification
#' for each of them using the specified database. The default is to use the
#'  GBIF database, but it can be changed to ITIS with the "sel_db"
#' parameter.
#' @param taxa_vec A character vector of taxa names to classify.
#' @param sel_db A string indicating which database to use. Default is "gbif".
#' @param verbose
#' Logical. If TRUE the additional messages are printed on the console.
#' @return A data frame with classification information for each taxa name.
#' @export
#' @examples
#' get_classification_buk(c("Homo sapiens", "Panthera tigris"))
get_classification_buk <- function(taxa_vec, sel_db = c("gbif", "itis"), verbose = FALSE) {
  sel_db <- match.arg(sel_db)

  # prealocate space
  data_taxa_res <-
    data.frame(
      sel_name = taxa_vec
    )

  # resolve taxa
  data_resolve <-
    taxize::resolve(data_taxa_res$sel_name) %>%
    purrr::pluck(1)

  if (
    nrow(data_resolve) == 0
  ) {
    return(data_taxa_res)
  }

  # select the resolve with heighest score
  data_resolve_best <-
    data_resolve %>%
    dplyr::group_by(
      dplyr::across("user_supplied_name")
    ) %>%
    # get the best match
    dplyr::filter(
      get("score") == max(get("score"))
    ) %>%
    dplyr::ungroup()

  # nest ataxa with their resolve
  data_taxa_res <-
    dplyr::left_join(
      data_taxa_res,
      data_resolve_best,
      by = dplyr::join_by("sel_name" == "user_supplied_name")
    ) %>%
    tidyr::nest(
      data_resolve = dplyr::any_of(names(data_resolve))
    )

  if (
    all(is.na(data_taxa_res$data_resolve))
  ) {
    return(data_taxa_res)
  }

  # get vector of unique resolved names
  data_resolve_unique <-
    data_taxa_res %>%
    tidyr::unnest(data_resolve) %>%
    dplyr::distinct(matched_name) %>%
    tidyr::drop_na(matched_name) %>%
    purrr::chuck("matched_name")

  # get id
  suppressWarnings(
    taxa_mached_name_id_check <-
      data_resolve_unique %>%
      taxize::get_gbifid_(
        messages = verbose
      )
  )

  # If there is nothing, return empty
  if (
    all(is.na(taxa_mached_name_id_check))
  ) {
    base::message("data does not find")

    return(data_taxa_res)
  }

  # turn it into a data frame
  data_taxa_mached_name_id_full <-
    taxa_mached_name_id_check %>%
    purrr::map(
      .f = ~ dplyr::slice(.x, 1)
    ) %>%
    dplyr::bind_rows(
      .id = "matched_name"
    )

  data_taxa_mached_name_id <-
    data_taxa_mached_name_id_full %>%
    dplyr::select(
      matched_name,
      id = usagekey)

  # get the most matching ID
  data_id <-
    data_taxa_res %>%
    tidyr::unnest(data_resolve) %>%
    dplyr::select(sel_name, matched_name) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      .,
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
    dplyr::select(!dplyr::any_of("Freq"))

  # save the most matching ID
  data_taxa_res <-
    data_taxa_res %>%
    dplyr::left_join(
      data_id,
      by = dplyr::join_by("sel_name")
    )

  # get_classification
  data_classification <-
    taxize::classification(
      sci_id = data_taxa_res$id,
      db = sel_db
    )

  names(data_classification) <-
    ifelse(is.na(data_taxa_res$id), "NA", data_taxa_res$id)

  data_classification_res <-
    data_classification %>%
    purrr::map(
      .f = ~ as.data.frame(.x)
    ) %>%
    dplyr::bind_rows(
      .id = "taxon_id"
    ) %>%
    dplyr::select(
      -dplyr::any_of(".x")
    ) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(
      dplyr::across("taxon_id")
    ) %>%
    tidyr::nest(
      classification = c("name", "rank", "id")
    ) %>%
    dplyr::ungroup()

  # process the classification
  data_taxa_res <-
    dplyr::left_join(
      data_taxa_res,
      data_classification_res,
      by = dplyr::join_by("id" == "taxon_id")
    )

  return(data_taxa_res)
}
