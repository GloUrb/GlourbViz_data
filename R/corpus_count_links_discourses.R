#' Compter les liens uniques du corpus
#'
#' Calcule le nombre de liens uniques par paire ville-rivière et type
#' de requête, ainsi que le nombre total de liens uniques par ville.
#'
#' @param corpus Corpus de résultats web.
#'
#' @return Table contenant le nombre de liens uniques par paire
#'   ville-rivière-query et par ville.
#' @export
create_corpus_count_links <- function(corpus) {
  
  corpus_city <- corpus |>
    dplyr::mutate(
      city_id = sub("_.*$", "", fid)
    )
  
  count_detail <- corpus_city |>
    dplyr::group_by(
      city_id,
      fid,
      urban_aggl,
      river,
      query
    ) |>
    dplyr::summarise(
      count = dplyr::n_distinct(link, na.rm = TRUE),
      .groups = "drop"
    )
  
  count_city <- corpus_city |>
    dplyr::group_by(city_id) |>
    dplyr::summarise(
      count_city = dplyr::n_distinct(link, na.rm = TRUE),
      .groups = "drop"
    )
  
  count_detail |>
    dplyr::left_join(
      count_city,
      by = "city_id"
    )
}