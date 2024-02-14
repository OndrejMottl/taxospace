#' @title Get classification for given vector of taxa
#' @description
#' This function takes a vector of taxa names and returns the classification
#' for each of them using the specified database. The default is to use the
#'  GBIF database, but it can be changed to ITIS with the "sel_db_class"
#' parameter. If the name is not found in either database,
#' an empty list is returned.
#' @param taxa_vec A character vector of taxa names to classify.
#' @param sel_db_name A string indicating which database to use to
#' resolve names. Default is "gnr".
#' @param sel_db_class A string indicating which database to use for
#' classification. Default is "gbif".
#' @param use_cache Logical. If TRUE the intermediate results are cached.
#' This speeds up the process drastically for large vectors. Default is TRUE.
#' @param verbose
#' Logical. If TRUE the additional messages are printed on the console.
#' @return A data.frame with the classification of the taxonomic
#' name, including the name, rank, and ID
#' @export
#' @examples
#' get_classification("Homo sapiens")
#' get_classification(c("Canis lupus", "Felis catus"))
#' get_classification("Pikachu")
#' @importFrom rlang .data
#' @seealso [taxize::resolve], [taxize::classification]
get_classification <- function(taxa_vec,
                               sel_db_name = c("gnr", "iplant"),
                               sel_db_class = c("gbif", "itis"),
                               use_cache = FALSE,
                               verbose = FALSE) {
  sel_db_name <- match.arg(sel_db_name)
  sel_db_class <- match.arg(sel_db_class)

  # Preallocate space
  data_taxa_res <-
    data.frame(sel_name = taxa_vec)

  #----------------------------------------------------------#
  # 1. resolve taxa -----
  #----------------------------------------------------------#

  data_resolve <-
    taxize::resolve(
      sci = data_taxa_res$sel_name,
      db = sel_db_name
    ) %>%
    purrr::pluck(1)

  if (
    nrow(data_resolve) == 0
  ) {
    return(data_taxa_res)
  }

  # Select the best resolve
  data_resolve_best <-
    select_best_resolve(data_resolve)

  # Nest taxa with their resolve
  data_taxa_res <-
    dplyr::left_join(
      data_taxa_res,
      data_resolve_best,
      by = c("sel_name" = "user_supplied_name")
    ) %>%
    tidyr::nest(
      data_resolve = dplyr::any_of(names(data_resolve))
    )

  if (
    all(is.na(data_taxa_res$data_resolve))
  ) {
    return(data_taxa_res)
  }

  # Get vector of unique resolved names
  data_resolve_unique <-
    data_taxa_res %>%
    tidyr::unnest("data_resolve") %>%
    dplyr::distinct(.data$matched_name) %>%
    tidyr::drop_na("matched_name") %>%
    purrr::pluck("matched_name")

  #--------------------------------------------------#
  # 1.1 resolve - ID -----
  #--------------------------------------------------#

  if (
    isTRUE(use_cache)
  ) {
    # preallocate space
    data_resolve_unique_present <- character()
    data_taxa_matched_name_id_loaded <- tibble::tibble()
    data_taxa_matched_name_id_accepted <- tibble::tibble()

    data_resolve_unique_present <-
      get_cached_file_names(sel_dir = "resolve")

    data_resolve_lookup <-
      tibble::tibble(
        name_resolve = data_resolve_unique
      ) %>%
      dplyr::mutate(
        name_clean = janitor::make_clean_names(.data$name_resolve, allow_dupes = TRUE)
      )

    if (
      length(data_resolve_unique_present) > 0
    ) {
      # Read the cache
      data_taxa_matched_name_id_loaded <-
        # only load the present data
        data_resolve_lookup %>%
        purrr::chuck("name_clean") %>%
        load_cached_resolved_names() %>%
        dplyr::left_join(
          data_resolve_lookup,
          by = c("name_clean")
        ) %>%
        dplyr::relocate(.data$name_resolve) %>%
        dplyr::rename(matched_name = .data$name_resolve)
    }

    data_resolve_to_run <-
      data.frame(
        name_resolve = data_resolve_unique
      ) %>%
      dplyr::left_join(
        data_resolve_lookup,
        by = c("name_resolve")
      ) %>%
      dplyr::filter(
        !.data$name_clean %in% data_resolve_unique_present
      ) %>%
      purrr::chuck("name_resolve")

    if (
      length(data_resolve_to_run) > 0
    ) {
      # Get ID
      suppressWarnings({
        taxa_matched_name_id_check <-
          data_resolve_to_run %>%
          taxize::get_gbifid_(messages = verbose)
      })

      data_taxa_matched_name_id_accepted <-
        taxa_matched_name_id_check %>%
        purrr::map(.f = ~ get_accepted_row(.x))

      # Cache the results
      cache_dataframe(
        data_vec = data_taxa_matched_name_id_accepted,
        sel_dir = "resolve",
        clean_name = TRUE
      )
    }

    data_taxa_matched_name_id_full <-
      dplyr::bind_rows(
        data_taxa_matched_name_id_accepted,
        .id = "matched_name"
      ) %>%
      dplyr::bind_rows(data_taxa_matched_name_id_loaded)
  } else {
    # Get ID
    suppressWarnings({
      taxa_matched_name_id_check <-
        data_resolve_unique %>%
        taxize::get_gbifid_(messages = verbose)
    })

    if (
      all(is.na(taxa_matched_name_id_check))
    ) {
      base::message("Data does not find")
      return(data_taxa_res)
    }

    data_taxa_matched_name_id_accepted <-
      taxa_matched_name_id_check %>%
      purrr::map(.f = ~ get_accepted_row(.x))

    data_taxa_matched_name_id_full <-
      dplyr::bind_rows(
        data_taxa_matched_name_id_accepted,
        .id = "matched_name"
      )
  }

  data_taxa_matched_name_id <-
    data_taxa_matched_name_id_full %>%
    dplyr::select(
      "matched_name",
      "usagekey"
    ) %>%
    dplyr::rename(
      id = "usagekey"
    )

  # Get the most matching ID
  data_id <-
    get_most_matching_id(data_taxa_res, data_taxa_matched_name_id)


  #----------------------------------------------------------#
  # 2. Classification -----
  #----------------------------------------------------------#

  if (
    isTRUE(use_cache)
  ) {
    # preallocate space
    data_classification <- list()
    data_classification_loaded <- list()
    data_classification_present <- character()

    id_unique <- unique(data_id$id)

    data_classification_present <-
      get_cached_file_names(sel_dir = "classification")

    if (
      length(data_classification_present) > 0
    ) {
      data_classification_to_load <-
        data_classification_present[data_classification_present %in% id_unique]

      data_classification_loaded <-
        load_cached_classification(data_classification_to_load)
    }

    id_to_run <-
      id_unique[!id_unique %in% data_classification_present]

    if (
      length(id_to_run) > 0
    ) {
      data_classification <-
        taxize::classification(
          sci_id = id_to_run,
          db = sel_db_class
        )

      names(data_classification) <-
        ifelse(is.na(id_to_run), "NA_character_", id_to_run)

      # Cache the results
      cache_dataframe(
        data_vec = data_classification,
        sel_dir = "classification",
        clean_name = FALSE
      )
    }

    data_classification <-
      c(
        data_classification_loaded,
        data_classification
      )
  } else {
    # get_classification
    data_classification <-
      taxize::classification(
        sci_id = data_id$id,
        db = sel_db_class
      )

    names(data_classification) <-
      ifelse(is.na(data_id$id), "NA", data_id$id)
  }

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
    data_taxa_res %>%
    dplyr::left_join(
      data_id,
      by = dplyr::join_by("sel_name")
    ) %>%
    dplyr::left_join(
      data_classification_res,
      by = dplyr::join_by("id" == "taxon_id")
    )

  return(data_taxa_res)
}
