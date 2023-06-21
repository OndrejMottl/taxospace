get_classification <- function(taxa, interactive = FALSE) {
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
      ),
      db = "gbif",
      id = NA_character_,
      classification = data.frame(
        name = NA_character_,
        rank = NA_character_,
        id = NA_character_
      )
    )

  # add taxa mname
  list_sel_taxa$sel_name <-
    taxa

  # resolve taxa
  data_taxon_resolve <-
    taxize::resolve(list_sel_taxa$sel_name) %>%
    purrr::pluck(1)

  # save the best match
  list_sel_taxa$data_resolve <-
    data_taxon_resolve %>%
    dplyr::filter(
      score == max(score)
    )

  # get id (GBIF)
  taxa_mached_name_id_check <-
    taxize::get_gbifid(
      sci = list_sel_taxa$data_resolve$matched_name,
      messages = FALSE,
      ask = interactive
    )

  # If there is nothing in GBIF, try ITIS
  if (
    all(is.na(taxa_mached_name_id_check))
  ) {
    taxa_mached_name_id_check <-
      taxize::get_tsn(
        sci = list_sel_taxa$data_resolve$matched_name,
        messages = FALSE,
        ask = interactive,
        accepted = FALSE
      )

    list_sel_taxa$db <- "itis"
  }

  # If there is nothing in ITIS, try return empty
  if (
    all(is.na(taxa_mached_name_id_check))
  ) {
    message("data does not find")

    return(list_sel_taxa)
  }

  # save the most matching ID
  list_sel_taxa$id <-
    taxa_mached_name_id_check %>%
    table() %>%
    as.data.frame() %>%
    dplyr::arrange(-Freq) %>%
    dplyr::slice(1) %>%
    dplyr::pull(1) %>%
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