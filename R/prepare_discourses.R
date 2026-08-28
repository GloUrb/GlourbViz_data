# PREPARATION DES DONNEES DE DISCOURS

#' Préparer le lexique de discours
#'
#' Le lexique conserve :
#' - en_word : libellé original du terme, utilisé pour l'affichage ;
#' - match_word : forme à rechercher dans lemmatext.
#'
#' Si match_word est absent du fichier source, en_word est utilisé
#' par défaut.
#'
#' @param lexique_brut Table contenant au minimum en_word
#'   et un identifiant fid_word ou fid.
#'
#' @return Un data.frame avec fid_word, en_word et match_word.
#' @export
prepare_lexicon <- function(lexique_brut) {
  
  if (!"en_word" %in% names(lexique_brut)) {
    stop("Le lexique doit contenir une colonne appelée en_word.")
  }
  
  if (!"match_word" %in% names(lexique_brut)) {
    lexique_brut <- lexique_brut |>
      dplyr::mutate(
        match_word = en_word
      )
  }
  
  if ("fid_word" %in% names(lexique_brut)) {
    
    lexique <- lexique_brut |>
      dplyr::transmute(
        fid_word = fid_word,
        en_word = stringr::str_to_lower(
          stringr::str_squish(en_word)
        ),
        match_word = stringr::str_to_lower(
          stringr::str_squish(match_word)
        )
      )
    
  } else if ("fid" %in% names(lexique_brut)) {
    
    lexique <- lexique_brut |>
      dplyr::transmute(
        fid_word = fid,
        en_word = stringr::str_to_lower(
          stringr::str_squish(en_word)
        ),
        match_word = stringr::str_to_lower(
          stringr::str_squish(match_word)
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
      !is.na(match_word),
      en_word != "",
      match_word != ""
    ) |>
    dplyr::distinct(
      fid_word,
      en_word,
      match_word
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
      city_id,
      fid,
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
      !is.na(city_id),
      !is.na(fid),
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
      city_id,
      riviere
    ) |>
    
    dplyr::summarise(
      
      # Les fid sont conservés pour la traçabilité
      fid = paste(
        sort(unique(fid)),
        collapse = " / "
      ),
      
      urban_aggl = paste(
        sort(unique(urban_aggl)),
        collapse = " / "
      ),
      
      ville = paste(
        sort(unique(ville)),
        collapse = " / "
      ),
      
      latitude = dplyr::first(latitude),
      longitude = dplyr::first(longitude),
      
      country_en = paste(
        sort(unique(country_en)),
        collapse = " / "
      ),
      
      country_fr = paste(
        sort(unique(country_fr)),
        collapse = " / "
      ),
      
      gl = paste(
        sort(unique(gl)),
        collapse = " / "
      ),
      
      .groups = "drop"
    )
}


#' Créer les informations descriptives par ville
#'
#' @param pages Pages préparées.
#'
#' @return Une ligne par city_id
#' @export
create_info_city <- function(pages) {
  
  pages |>
    
    dplyr::group_by(city_id) |>
    
    dplyr::summarise(
      urban_aggl = paste(
        sort(unique(urban_aggl)),
        collapse = "/ "
      ),
      ville = paste(
        sort(unique(ville)),
        collapse = " / "
      ),
      
      latitude = dplyr::first(latitude),
      longitude = dplyr::first(longitude),
      
      country_en = paste(
        sort(unique(country_en)),
        collapse = " / "
      ),
      
      country_fr = paste(
        sort(unique(country_fr)),
        collapse = " / "
      ),
      
      gl = paste(
        sort(unique(gl)),
        collapse = " / "
      ),
      
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
      city_id,
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
      city_id,
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
      city_id,
      fid,
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


#' Repérer les termes du lexique dans les textes
#'
#' Cette fonction gère à la fois :
#' - les termes simples : drought ;
#' - les expressions multi-mots : climate change,
#'   extreme weather event
#'
#' La correspondance est effectuée avec match_word, tandis que
#' la colonne word retournée conserve en_word comme libellé
#' original du lexique.
#'
#' Pour une expression multi-mots, token_start et token_end
#' correspondent respectivement au premier et au dernier token
#' de l'expression.
#'
#' @param tokens Tokens produits par tokenize_discourses().
#' @param lexique Lexique préparé par prepare_lexicon().
#'
#' @return Une ligne par occurrence d'un terme du lexique.
#' @export
match_lexicon <- function(tokens, lexique) {
  
  required_cols <- c(
    "fid_word",
    "en_word",
    "match_word"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(lexique)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Colonnes manquantes dans le lexique :",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  

  # Tokenisation du lexique avec la MEME logique que le corpus

  
  lexicon_tokens <- lexique |>
    
    dplyr::select(
      fid_word,
      en_word,
      match_word
    ) |>
    
    dplyr::mutate(
      match_word_to_tokenize = match_word
    ) |>
    
    tidytext::unnest_tokens(
      output = match_token,
      input = match_word_to_tokenize,
      token = "words",
      to_lower = TRUE
    ) |>
    
    dplyr::group_by(
      fid_word,
      en_word,
      match_word
    ) |>
    
    dplyr::mutate(
      match_index = dplyr::row_number(),
      n_tokens = dplyr::n()
    ) |>
    
    dplyr::ungroup()
  
  

  # 1. TERMES SIMPLES

  
  single_lookup <- lexicon_tokens |>
    
    dplyr::filter(
      n_tokens == 1
    ) |>
    
    dplyr::select(
      fid_word,
      en_word,
      match_word,
      match_token,
      n_tokens
    )
  
  
  single_matches <- tokens |>
    
    dplyr::inner_join(
      single_lookup,
      by = c(
        "word" = "match_token"
      )
    ) |>
    
    dplyr::transmute(
      fid_word,
      word = en_word,
      match_word,
      
      page_id,
      citycode,
      city_id,
      fid,
      riviere,
      hl,
      query,
      link,
      
      token_start = token_position,
      token_end = token_position,
      
      n_tokens
    )
  
  

  # 2. EXPRESSIONS MULTI-MOTS

  
  multi_terms <- lexicon_tokens |>
    
    dplyr::filter(
      n_tokens > 1
    ) |>
    
    dplyr::group_by(
      fid_word,
      en_word,
      match_word,
      n_tokens
    ) |>
    
    dplyr::summarise(
      match_tokens = list(match_token),
      .groups = "drop"
    )
  
  
  if (nrow(multi_terms) == 0) {
    
    multi_matches <- single_matches[0, ]
    
  } else {
    
    # On réduit d'abord le gros tableau de tokens aux mots
    # susceptibles d'être utilisés dans une expression.
    useful_multi_tokens <- unique(
      unlist(multi_terms$match_tokens)
    )
    
    
    token_pool <- tokens |>
      
      dplyr::filter(
        word %in% useful_multi_tokens
      ) |>
      
      dplyr::select(
        page_id,
        city_id,
        fid,
        citycode,
        riviere,
        hl,
        query,
        link,
        token_position,
        word
      )
    
    
    multi_matches_list <- lapply(
      seq_len(nrow(multi_terms)),
      function(i) {
        
        current_term <- multi_terms[i, ]
        
        parts <- current_term$match_tokens[[1]]
        
        current_n_tokens <- length(parts)
        
        
        # Candidats = positions où apparaît le premier token
        candidates <- token_pool |>
          
          dplyr::filter(
            word == parts[1]
          ) |>
          
          dplyr::transmute(
            page_id,
            city_id,
            fid,
            citycode,
            riviere,
            hl,
            query,
            link,
            token_start = token_position
          )
        
        
        if (nrow(candidates) == 0) {
          return(single_matches[0, ])
        }
        
        
        # Vérification des tokens suivants aux positions
        # immédiatement consécutives.
        if (current_n_tokens >= 2) {
          
          for (j in 2:current_n_tokens) {
            
            next_positions <- token_pool |>
              
              dplyr::filter(
                word == parts[j]
              ) |>
              
              dplyr::transmute(
                page_id,
                token_start =
                  token_position - (j - 1)
              )
            
            
            candidates <- candidates |>
              
              dplyr::inner_join(
                next_positions,
                by = c(
                  "page_id",
                  "token_start"
                )
              )
            
            
            if (nrow(candidates) == 0) {
              break
            }
          }
        }
        
        
        if (nrow(candidates) == 0) {
          return(single_matches[0, ])
        }
        
        
        candidates |>
          
          dplyr::transmute(
            fid_word =
              current_term$fid_word[[1]],
            
            word =
              current_term$en_word[[1]],
            
            match_word =
              current_term$match_word[[1]],
            
            page_id,
            city_id,
            fid,
            citycode,
            riviere,
            hl,
            query,
            link,
            
            token_start,
            
            token_end =
              token_start + current_n_tokens - 1,
            
            n_tokens =
              current_n_tokens
          )
      }
    )
    
    
    multi_matches <- dplyr::bind_rows(
      multi_matches_list
    )
  }
  
  

  # 3. UNION DES TERMES SIMPLES ET MULTI-MOTS

  
  dplyr::bind_rows(
    single_matches,
    multi_matches
  ) |>
    
    dplyr::arrange(
      page_id,
      token_start,
      fid_word
    )
}
