# CONTEXTE LEXICAL


#' Créer les tokens de contexte autour des termes du lexique
#'
#' La fenêtre est calculée autour des BORNES du terme :
#'
#' - pour un terme simple, token_start = token_end ;
#' - pour une expression multi-mots, les mots de l'expression
#'   sont entièrement exclus du contexte.
#'
#' Exemple avec window = 5 :
#'
#' mot5 ... mot1 [EXTREME WEATHER EVENT] mot1 ... mot5
#'
#' @param tokens Tokens complets.
#' @param tokens_lexique Occurrences des termes du lexique.
#' @param window Taille de la fenêtre de part et d'autre.
#'
#' @return Tokens de contexte nettoyés.
#' @export
create_context_tokens <- function(
    tokens,
    tokens_lexique,
    window = 5
) {
  
  if (window < 1) {
    stop("window doit être supérieur ou égal à 1.")
  }
  
  
  lexicon_occurrences <- tokens_lexique |>
    
    dplyr::transmute(
      occurrence_id = dplyr::row_number(),
      
      fid_word,
      
      lexicon_word = word,
      
      match_word,
      
      page_id,
      city_id,
      fid,
      citycode,
      riviere,
      
      hl,
      query,
      
      token_start,
      token_end
    )
  
  

  # CONTEXTE AVANT LE TERME

  
  before_offsets <- tibble::tibble(
    distance = seq(
      -window,
      -1
    )
  )
  
  
  context_before <- tidyr::crossing(
    lexicon_occurrences,
    before_offsets
  ) |>
    
    dplyr::mutate(
      context_position =
        token_start + distance
    ) |>
    
    dplyr::filter(
      context_position > 0
    )
  
  

  # CONTEXTE APRES LE TERME

  
  after_offsets <- tibble::tibble(
    distance = seq(
      1,
      window
    )
  )
  
  
  context_after <- tidyr::crossing(
    lexicon_occurrences,
    after_offsets
  ) |>
    
    dplyr::mutate(
      context_position =
        token_end + distance
    )
  
  

  # UNION AVANT + APRES

  
  context_positions <- dplyr::bind_rows(
    context_before,
    context_after
  )
  
  

  # RECUPERATION DES MOTS AUX POSITIONS CALCULEES

  
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
  
  

  # STOPWORDS

  
  stop_words_en <- tidytext::stop_words |>
    
    dplyr::filter(
      lexicon == "snowball"
    ) |>
    
    dplyr::distinct(word) |>
    
    dplyr::rename(
      context_word = word
    )
  
  

  # NETTOYAGE FINAL

  
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
      
      # Utile surtout pour les termes simples.
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
      
      city_id,
      fid,
      citycode,
      riviere,
      
      hl,
      query
    ) |>
    
    dplyr::summarise(
      nb_cooccurrences = dplyr::n(),
      
      # Distance moyenne au bord le plus proche du terme.
      mean_abs_distance =
        mean(abs(distance)),
      
      # Distance minimale au bord le plus proche du terme.
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
      
      city_id,
      fid,
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
      city_id,
      riviere,
      page_id,
      dplyr::desc(nb_cooccurrences)
    )
}
