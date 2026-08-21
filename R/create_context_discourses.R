# CONTEXTE LEXICAL

#' Créer les tokens de contexte autour des mots du lexique
#'
#' @param tokens Tokens complets.
#' @param tokens_lexique Occurrences des mots du lexique.
#' @param window Taille de la fenêtre de part et d'autre.
#'
#' @return Tokens de contexte nettoyés.
#' @export
create_context_tokens <- function(
    tokens,
    tokens_lexique,
    window = 5
) {
  
  lexicon_occurrences <- tokens_lexique |>
    
    dplyr::transmute(
      occurrence_id = dplyr::row_number(),
      
      fid_word,
      
      lexicon_word = word,
      
      page_id,
      
      citycode,
      riviere,
      
      hl,
      query,
      
      occurrence_position = token_position
    )
  
  
  context_offsets <- tibble::tibble(
    distance = c(
      seq(-window, -1),
      seq(1, window)
    )
  )
  
  
  context_positions <- tidyr::crossing(
    lexicon_occurrences,
    context_offsets
  ) |>
    
    dplyr::mutate(
      context_position =
        occurrence_position + distance
    ) |>
    
    dplyr::filter(
      context_position > 0
    )
  
  
  context_tokens <- context_positions |>
    
    dplyr::inner_join(
      tokens |>
        
        dplyr::select(
          page_id,
          token_position,
          context_word = word
        ),
      
      by = c(
        "page_id",
        "context_position" = "token_position"
      )
    )
  
  
  stop_words_en <- tidytext::stop_words |>
    
    dplyr::filter(
      lexicon == "snowball"
    ) |>
    
    dplyr::distinct(word) |>
    
    dplyr::rename(
      context_word = word
    )
  
  
  context_tokens |>
    
    dplyr::anti_join(
      stop_words_en,
      by = "context_word"
    ) |>
    
    dplyr::filter(
      stringr::str_detect(
        context_word,
        "^[[:alpha:]]+$"
      ),
      
      stringr::str_length(
        context_word
      ) >= 3,
      
      context_word != lexicon_word
    )
}



#' Créer la table détaillée de contexte
#'
#' @param context_tokens_clean Tokens de contexte nettoyés.
#' @param pages Pages préparées.
#'
#' @return Table context.
#' @export
create_context_table <- function(
    context_tokens_clean,
    pages
) {
  
  context_tokens_clean |>
    
    dplyr::group_by(
      fid_word,
      lexicon_word,
      context_word,
      
      page_id,
      
      citycode,
      riviere,
      
      hl,
      query
    ) |>
    
    dplyr::summarise(
      nb_cooccurrences = dplyr::n(),
      
      mean_abs_distance =
        mean(abs(distance)),
      
      min_abs_distance =
        min(abs(distance)),
      
      before_count =
        sum(distance < 0),
      
      after_count =
        sum(distance > 0),
      
      .groups = "drop"
    ) |>
    
    dplyr::left_join(
      pages |>
        
        dplyr::select(
          page_id,
          
          urban_aggl,
          ville,
          
          position,
          
          title,
          link,
          domain,
          displayed_link,
          
          latitude,
          longitude,
          
          country_en,
          country_fr,
          gl,
          
          hl1,
          hl2,
          hl3,
          hl4,
          hl5,
          hl6,
          
          num_page
        ),
      
      by = "page_id"
    ) |>
    
    dplyr::select(
      fid_word,
      
      lexicon_word,
      context_word,
      
      page_id,
      
      citycode,
      urban_aggl,
      ville,
      riviere,
      
      country_en,
      country_fr,
      
      latitude,
      longitude,
      
      gl,
      
      hl,
      
      hl1,
      hl2,
      hl3,
      hl4,
      hl5,
      hl6,
      
      query,
      
      position,
      num_page,
      
      title,
      link,
      domain,
      displayed_link,
      
      nb_cooccurrences,
      
      mean_abs_distance,
      min_abs_distance,
      
      before_count,
      after_count
    ) |>
    
    dplyr::arrange(
      fid_word,
      citycode,
      riviere,
      page_id,
      dplyr::desc(nb_cooccurrences)
    )
}