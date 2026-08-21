# TABLES LEXICALES VILLE-RIVIERE ET VILLE


#' Créer la table lexicale par paire ville-rivière
#'
#' @param tokens_lexique Occurrences des mots du lexique.
#' @param pages_city_river Totaux de pages analysables.
#' @param info_city_river Informations descriptives.
#'
#' @return Table lexicon_discourses_city_river.
#' @export
create_lexicon_city_river <- function(
    tokens_lexique,
    pages_city_river,
    info_city_river
) {
  
  tokens_lexique |>
    
    dplyr::group_by(
      fid_word,
      word,
      citycode,
      riviere,
      hl,
      query
    ) |>
    
    dplyr::summarise(
      nb_occurrences = dplyr::n(),
      
      nb_pages_concerned =
        dplyr::n_distinct(link),
      
      .groups = "drop"
    ) |>
    
    dplyr::left_join(
      pages_city_river,
      
      by = c(
        "citycode",
        "riviere",
        "hl",
        "query"
      )
    ) |>
    
    dplyr::left_join(
      info_city_river,
      
      by = c(
        "citycode",
        "riviere"
      )
    ) |>
    
    dplyr::mutate(
      presence = TRUE,
      
      freq_pages = dplyr::if_else(
        nb_pages_total_analysable > 0,
        
        nb_pages_concerned /
          nb_pages_total_analysable,
        
        NA_real_
      ),
      
      occ_par_page_concernee = dplyr::if_else(
        nb_pages_concerned > 0,
        
        nb_occurrences /
          nb_pages_concerned,
        
        NA_real_
      ),
      
      occ_par_page_total = dplyr::if_else(
        nb_pages_total_analysable > 0,
        
        nb_occurrences /
          nb_pages_total_analysable,
        
        NA_real_
      )
    ) |>
    
    dplyr::select(
      fid_word,
      word,
      
      citycode,
      urban_aggl,
      ville,
      riviere,
      
      latitude,
      longitude,
      
      country_en,
      country_fr,
      gl,
      
      hl,
      query,
      
      presence,
      
      nb_occurrences,
      
      nb_pages_concerned,
      nb_pages_total_analysable,
      
      freq_pages,
      
      occ_par_page_concernee,
      occ_par_page_total
    ) |>
    
    dplyr::arrange(
      fid_word,
      citycode,
      riviere,
      query,
      hl
    )
}



#' Créer la table lexicale par ville
#'
#' @param tokens_lexique Occurrences du lexique.
#' @param pages_city Totaux par ville.
#' @param info_city Informations descriptives des villes.
#'
#' @return Table lexicon_discourses_city.
#' @export
create_lexicon_city <- function(
    tokens_lexique,
    pages_city,
    info_city
) {
  
  tokens_lexique |>
    
    dplyr::group_by(
      fid_word,
      word,
      citycode,
      hl,
      query
    ) |>
    
    dplyr::summarise(
      nb_occurrences = dplyr::n(),
      
      nb_pages_concerned =
        dplyr::n_distinct(link),
      
      nb_rivieres_concernees =
        dplyr::n_distinct(riviere),
      
      .groups = "drop"
    ) |>
    
    dplyr::left_join(
      pages_city,
      
      by = c(
        "citycode",
        "hl",
        "query"
      )
    ) |>
    
    dplyr::left_join(
      info_city,
      
      by = "citycode"
    ) |>
    
    dplyr::mutate(
      presence = TRUE,
      
      freq_pages = dplyr::if_else(
        nb_pages_total_analysable > 0,
        
        nb_pages_concerned /
          nb_pages_total_analysable,
        
        NA_real_
      ),
      
      freq_rivieres = dplyr::if_else(
        nb_rivieres_total > 0,
        
        nb_rivieres_concernees /
          nb_rivieres_total,
        
        NA_real_
      ),
      
      occ_par_page_concernee = dplyr::if_else(
        nb_pages_concerned > 0,
        
        nb_occurrences /
          nb_pages_concerned,
        
        NA_real_
      ),
      
      occ_par_page_total = dplyr::if_else(
        nb_pages_total_analysable > 0,
        
        nb_occurrences /
          nb_pages_total_analysable,
        
        NA_real_
      ),
      
      occ_par_riviere = dplyr::if_else(
        nb_rivieres_total > 0,
        
        nb_occurrences /
          nb_rivieres_total,
        
        NA_real_
      )
    ) |>
    
    dplyr::select(
      fid_word,
      word,
      
      citycode,
      urban_aggl,
      ville,
      
      latitude,
      longitude,
      
      country_en,
      country_fr,
      gl,
      
      hl,
      query,
      
      presence,
      
      nb_occurrences,
      
      nb_pages_concerned,
      nb_pages_total_analysable,
      
      nb_rivieres_concernees,
      nb_rivieres_total,
      
      freq_pages,
      freq_rivieres,
      
      occ_par_page_concernee,
      occ_par_page_total,
      
      occ_par_riviere
    ) |>
    
    dplyr::arrange(
      fid_word,
      citycode,
      query,
      hl
    )
}