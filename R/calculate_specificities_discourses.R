# SPECIFICITES TEXTOMETRIQUES


#' Calculer les spécificités globales des mots de contexte
#'
#' Le corpus de référence correspond à l'ensemble des mots
#' situés autour de tous les mots du lexique.
#'
#' @param context_tokens_clean Tokens de contexte nettoyés.
#' @param lexique Lexique préparé.
#'
#' @return Table des spécificités globales.
#' @export
calculate_context_specificities <- function(
    context_tokens_clean,
    lexique
) {
  
  mixr::tidy_specificities(
    context_tokens_clean,
    context_word,
    lexicon_word
  ) |>
    
    dplyr::left_join(
      lexique |>
        
        dplyr::transmute(
          fid_word,
          lexicon_word = en_word
        ),
      
      by = "lexicon_word"
    ) |>
    
    dplyr::select(
      fid_word,
      lexicon_word,
      context_word,
      spec,
      n
    ) |>
    
    dplyr::arrange(
      fid_word,
      dplyr::desc(spec),
      dplyr::desc(n)
    )
}



#' Sélectionner les spécificités utilisées par l'application
#'
#' @param specificities Table issue de
#'   calculate_context_specificities().
#' @param spec_min Seuil minimum de spécificité.
#' @param n_min Nombre minimum d'occurrences.
#'
#' @return Table filtrée.
#' @export
select_context_specificities <- function(
    specificities,
    spec_min = 2,
    n_min = 5
) {
  
  specificities |>
    
    dplyr::filter(
      spec >= spec_min,
      n >= n_min
    )
}