
# BUILD DISCOURSES DATA
# GloUrbViz


# 0. CHARGEMENT DU PACKAGE EN DEVELOPPEMENT


# À installer une seule fois
# install.packages("devtools")

devtools::load_all()


# 1. LECTURE DES DONNEES SOURCES


txt_page_work <- readRDS(
  "data-raw/discourses/txt_page_work.rds"
)


lexique_brut <- readxl::read_excel(
  "data-raw/discourses/lexique_discourses.xlsx"
)


# 2. PREPARATION


lexique <- prepare_lexicon(
  lexique_brut
)


pages <- prepare_pages(
  txt_page_work
)


cat(
  "Nombre de mots/termes dans le lexique :",
  nrow(lexique),
  "\n"
)

cat(
  "Nombre de pages analysables :",
  nrow(pages),
  "\n"
)

cat(
  "Nombre de villes :",
  dplyr::n_distinct(pages$citycode),
  "\n"
)

cat(
  "Nombre de paires ville-rivière :",
  pages |>
    dplyr::distinct(
      citycode,
      riviere
    ) |>
    nrow(),
  "\n"
)

cat(
  "Termes dont match_word diffère de en_word :",
  lexique |>
    dplyr::filter(
      en_word != match_word
    ) |>
    nrow(),
  "\n"
)


# 3. INFORMATIONS DESCRIPTIVES


info_city_river <- create_info_city_river(
  pages
)


info_city <- create_info_city(
  pages
)


page_totals <- create_page_totals(
  pages
)


pages_city_river <-
  page_totals$city_river


pages_city <-
  page_totals$city



# 4. TOKENISATION


tokens <- tokenize_discourses(
  pages
)


cat(
  "Nombre total de tokens :",
  nrow(tokens),
  "\n"
)


tokens_lexique <- match_lexicon(
  tokens,
  lexique
)


cat(
  "Occurrences de mots du lexique :",
  nrow(tokens_lexique),
  "\n"
)

nb_termes_detectes <- tokens_lexique |>
  dplyr::distinct(fid_word) |>
  nrow()


cat(
  "Nombre de termes du lexique détectés :",
  nb_termes_detectes,
  "/",
  nrow(lexique),
  "\n"
)


termes_non_detectes <- lexique |>
  
  dplyr::anti_join(
    tokens_lexique |>
      dplyr::distinct(fid_word),
    by = "fid_word"
  ) |>
  
  dplyr::select(
    fid_word,
    en_word,
    match_word
  )


if (nrow(termes_non_detectes) > 0) {
  
  cat(
    "\nTermes non détectés dans le corpus :\n"
  )
  
  print(
    termes_non_detectes,
    n = Inf
  )
}



# 5. TABLE VILLE-RIVIERE


lexicon_discourses_city_river <-
  create_lexicon_city_river(
    tokens_lexique,
    pages_city_river,
    info_city_river
  )



# 6. TABLE VILLE


lexicon_discourses_city <-
  create_lexicon_city(
    tokens_lexique,
    pages_city,
    info_city
  )



# 7. CONTEXTES +/- 5 MOTS


context_tokens_clean <-
  create_context_tokens(
    tokens = tokens,
    tokens_lexique = tokens_lexique,
    window = 5
  )


context <-
  create_context_table(
    context_tokens_clean,
    pages
  )



# 8. SPECIFICITES


context_specificities_global <-
  calculate_context_specificities(
    context_tokens_clean,
    lexique
  )


context_specificities_selected <-
  select_context_specificities(
    context_specificities_global,
    spec_min = 2,
    n_min = 5
  )



# 9. CONTROLES


cat(
  "\n========== TABLES FINALES ==========\n"
)

cat(
  "lexicon_discourses_city_river :",
  nrow(lexicon_discourses_city_river),
  "\n"
)

cat(
  "lexicon_discourses_city :",
  nrow(lexicon_discourses_city),
  "\n"
)

cat(
  "context :",
  nrow(context),
  "\n"
)

cat(
  "context_specificities_global :",
  nrow(context_specificities_global),
  "\n"
)

cat(
  "context_specificities_selected :",
  nrow(context_specificities_selected),
  "\n"
)



# 10. CONTROLE DES DOUBLONS


doublons_city_river <-
  lexicon_discourses_city_river |>
  
  dplyr::count(
    fid_word,
    citycode,
    riviere,
    hl,
    query,
    name = "n"
  ) |>
  
  dplyr::filter(
    n > 1
  )


doublons_city <-
  lexicon_discourses_city |>
  
  dplyr::count(
    fid_word,
    citycode,
    hl,
    query,
    name = "n"
  ) |>
  
  dplyr::filter(
    n > 1
  )


cat(
  "Doublons city-river :",
  nrow(doublons_city_river),
  "\n"
)

cat(
  "Doublons city :",
  nrow(doublons_city),
  "\n"
)


# Stop automatique si anomalie

if (nrow(doublons_city_river) > 0) {
  stop(
    "Des doublons anormaux existent dans la table city-river."
  )
}


if (nrow(doublons_city) > 0) {
  stop(
    "Des doublons anormaux existent dans la table city."
  )
}



# 11. EXEMPLE DROUGHT


exemple_drought <-
  context_specificities_selected |>
  
  dplyr::filter(
    lexicon_word == "drought"
  ) |>
  
  dplyr::slice_max(
    order_by = spec,
    n = 30,
    with_ties = FALSE
  )


print(
  exemple_drought
)

# Contrôle explicite de quelques expressions multi-mots.
controle_multiword <- tokens_lexique |>
  
  dplyr::filter(
    word %in% c(
      "water quality",
      "climate change",
      "public health",
      "seasonal flooding",
      "extreme weather events"
    )
  ) |>
  
  dplyr::count(
    fid_word,
    word,
    match_word,
    sort = TRUE
  )


cat(
  "\nContrôle des expressions multi-mots :\n"
)

print(
  controle_multiword,
  n = Inf
)


# 12. SAUVEGARDE


saveRDS(
  lexicon_discourses_city_river,
  "data/glourbviz_lexicon_discourses_city_river.rds"
)


saveRDS(
  lexicon_discourses_city,
  "data/glourbviz_lexicon_discourses_city.rds"
)


saveRDS(
  context,
  "data/glourbviz_context.rds"
)


saveRDS(
  context_specificities_global,
  "data/glourbviz_context_specificities_global.rds"
)


saveRDS(
  context_specificities_selected,
  "data/glourbviz_context_specificities_selected.rds"
)


cat(
  "\nTraitement terminé et données sauvegardées.\n"
)