# SPECIFICITES TEXTOM


#' Calculer les spécificités globales des mots de contexte
#'
#' Le corpus de référence correspond à l'ensemble des mots
#' situés autour de tous les mots du lexique, qu'ils soient
#' simples ou composés de plusieurs mots.
#' 
#' lexicon_word conserve le libellé original en_word.

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
#' La fonction peut être appliquée aux spécificités globales
#' ou aux spécificités calculées par ville.
#'
#' @param specificities Table de spécificités contenant
#'   au minimum spec et n.
#'   
#' @param spec_min Seuil minimum de spécificité.(avant il y avait aussi un param de minimum de n mais supp)
#'
#' @return Table filtrée.
#' @export
select_context_specificities <- function(
    specificities,
    spec_min = 2
) {
  
  specificities |>
    
    dplyr::filter(
      spec >= spec_min
    )
}



#' Calculer les spécificités des mots de contexte par ville
#'
#' Les spécificités sont calculées séparément pour chaque ville.
#' Le corpus de référence d'une ville correspond à l'ensemble
#' des mots de contexte situés autour de tous les termes du lexique
#' dans cette ville.
#' 
#' La table retournée conserve l'ensemble des combinaisons calculées.
#' La colonne n peut être NA lorsqu'un mot de contexte n'est pas
#' observé autour d'un terme du lexique dans une ville donnée.
#'
#' @param context_tokens_clean Tokens de contexte nettoyés,
#'   contenant notamment city_id.
#' @param lexique Lexique préparé.
#'
#' @return Table des spécificités par ville.
#' @export
calculate_context_specificities_city <- function(
    context_tokens_clean,
    lexique
) {
  
  context_tokens_clean |>
    
    dplyr::group_by(city_id) |>
    
    dplyr::group_modify(
      ~ mixr::tidy_specificities(
        .x,
        context_word,
        lexicon_word
      )
    ) |>
    
    dplyr::ungroup() |>
    
    dplyr::left_join(
      lexique |>
        dplyr::transmute(
          fid_word,
          lexicon_word = en_word
        ),
      
      by = "lexicon_word"
    ) |>
    
    dplyr::select(
      city_id,
      fid_word,
      lexicon_word,
      context_word,
      spec,
      n
    ) |>
    
    dplyr::arrange(
      city_id,
      fid_word,
      dplyr::desc(spec),
      dplyr::desc(n)
    )
}