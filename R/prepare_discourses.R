# PREPARATION DES DONNEES DE DISCOURS

#' Préparer le lexique de discours
#'
#' @param lexique_brut Table contenant au minimum en_word
#'   et un identifiant fid_word ou fid.
#'
#' @return Un data.frame avec fid_word et en_word.
#' @export
prepare_lexicon <- function(lexique_brut) {
  
  if ("fid_word" %in% names(lexique_brut)) {
    
    lexique <- lexique_brut |>
      dplyr::transmute(
        fid_word = fid_word,
        en_word = stringr::str_to_lower(
          stringr::str_trim(en_word)
        )
      )
    
  } else if ("fid" %in% names(lexique_brut)) {
    
    lexique <- lexique_brut |>
      dplyr::transmute(
        fid_word = fid,
        en_word = stringr::str_to_lower(
          stringr::str_trim(en_word)
        )
      )
    
  } else {
    
    stop(
      "Le lexique doit contenir une colonne appelée fid_word ou fid."
    )
  }
  
  
  lexique |>
    dplyr::filter(
      !is.na(fid_word),
      !is.na(en_word),
      en_word != ""
    ) |>
    dplyr::distinct(
      fid_word,
      en_word
    )
}



#' Préparer les pages web analysables
#'
#' Seules les pages possédant un lemmatext non vide sont
#' conservées.
#'
#' @param txt_page_work Table source des pages web.
#'
#' @return Table des pages analysables avec page_id.
#' @export
prepare_pages <- function(txt_page_work) {
  
  txt_page_work |>
    
    dplyr::select(
      citycode,
      urban_aggl,
      ville,
      riviere,
      
      X,
      position,
      
      title,
      link,
      domain,
      displayed_link,
      
      snippet,
      trans_snippet,
      
      text,
      text_en,
      tokenized_text,
      tokenized_noloc,
      
      hl,
      query,
      
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
      
      city_en,
      city_hl1,
      city_hl2,
      city_hl3,
      city_hl4,
      city_hl5,
      city_hl6,
      
      river_en,
      river_hl1,
      river_hl2,
      river_hl3,
      river_hl4,
      river_hl5,
      river_hl6,
      
      num_page,
      
      lemmatext
    ) |>
    
    dplyr::filter(
      !is.na(citycode),
      !is.na(riviere),
      !is.na(lemmatext),
      stringr::str_trim(lemmatext) != ""
    ) |>
    
    dplyr::mutate(
      page_id = dplyr::row_number()
    )
}



#' Créer les informations descriptives ville-rivière
#'
#' @param pages Pages préparées avec prepare_pages().
#'
#' @return Une ligne par paire citycode-riviere.
#' @export
create_info_city_river <- function(pages) {
  
  pages |>
    
    dplyr::group_by(
      citycode,
      riviere
    ) |>
    
    dplyr::summarise(
      urban_aggl = dplyr::first(urban_aggl),
      ville = dplyr::first(ville),
      
      latitude = dplyr::first(latitude),
      longitude = dplyr::first(longitude),
      
      country_en = dplyr::first(country_en),
      country_fr = dplyr::first(country_fr),
      
      gl = dplyr::first(gl),
      
      .groups = "drop"
    )
}



#' Créer les informations descriptives par ville
#'
#' @param pages Pages préparées.
#'
#' @return Une ligne par citycode.
#' @export
create_info_city <- function(pages) {
  
  pages |>
    
    dplyr::group_by(citycode) |>
    
    dplyr::summarise(
      urban_aggl = dplyr::first(urban_aggl),
      ville = dplyr::first(ville),
      
      latitude = dplyr::first(latitude),
      longitude = dplyr::first(longitude),
      
      country_en = dplyr::first(country_en),
      country_fr = dplyr::first(country_fr),
      
      gl = dplyr::first(gl),
      
      .groups = "drop"
    )
}



#' Calculer les volumes de pages analysables
#'
#' @param pages Pages préparées.
#'
#' @return Une liste contenant pages_city_river et pages_city.
#' @export
create_page_totals <- function(pages) {
  
  pages_city_river <- pages |>
    
    dplyr::group_by(
      citycode,
      riviere,
      hl,
      query
    ) |>
    
    dplyr::summarise(
      nb_pages_total_analysable =
        dplyr::n_distinct(link),
      
      .groups = "drop"
    )
  
  
  pages_city <- pages |>
    
    dplyr::group_by(
      citycode,
      hl,
      query
    ) |>
    
    dplyr::summarise(
      nb_pages_total_analysable =
        dplyr::n_distinct(link),
      
      nb_rivieres_total =
        dplyr::n_distinct(riviere),
      
      .groups = "drop"
    )
  
  
  list(
    city_river = pages_city_river,
    city = pages_city
  )
}



#' Tokeniser les textes lemmatisés
#'
#' @param pages Pages préparées.
#'
#' @return Une ligne par token avec sa position dans la page.
#' @export
tokenize_discourses <- function(pages) {
  
  pages |>
    
    dplyr::select(
      page_id,
      citycode,
      riviere,
      hl,
      query,
      link,
      lemmatext
    ) |>
    
    tidytext::unnest_tokens(
      output = word,
      input = lemmatext,
      token = "words",
      to_lower = TRUE
    ) |>
    
    dplyr::filter(
      !is.na(word),
      word != ""
    ) |>
    
    dplyr::group_by(page_id) |>
    
    dplyr::mutate(
      token_position = dplyr::row_number()
    ) |>
    
    dplyr::ungroup()
}



#' Repérer les mots du lexique
#'
#' @param tokens Tokens produits par tokenize_discourses().
#' @param lexique Lexique préparé.
#'
#' @return Tokens correspondant aux mots du lexique.
#' @export
match_lexicon <- function(tokens, lexique) {
  
  tokens |>
    
    dplyr::inner_join(
      lexique,
      by = c(
        "word" = "en_word"
      )
    )
}