#' Title : ajouter FID et city_id à txt_page_work
#' 
#'  Reconstruit le fid à partir de la colonne 'id' en utilisant dans txt_page_work
#' les fid officiels de list_city_river, puis crée city_id
#' à partir de la partie du fid située avant le premier "_".
#' (En lien avec le fait qu'il y ai quelques erreurs sur citycode)
#'
#' @param txt_page_work 
#' @param list_city_river Table de référence des paires ville-rivière.
#'
#' @returns Table txt_page_work enrichie avec fid et city_id.
#' @export
#'
#' @examples citycode melbourne(USA) = melbourne(Aust) = 2911545_206168 alors que pas la même agglo... 
#'           donc fid dans transformation de "id" en "fid" qu'on retrouve dans (list_city_river)
#'           mais on va prendre que les premiers chiffres avant '_' car fid (paire riviere/ville)
#'           le premier morceau de code = agglo --> melbourne (usa) 282_2_1 --> city_id = 282
#'           
create_page_ids <- function(txt_page_work, list_city_river) {
  
  # Liste officielle des 373 fid
  valid_fid <- list_city_river |>
    dplyr::distinct(fid) |>
    dplyr::pull(fid)
  
  # On cherche d'abord les fid les plus longs
  # Ex. 282_2 avant 282
  valid_fid <- valid_fid[
    order(nchar(valid_fid), decreasing = TRUE)
  ]
  
  fid_pattern <- paste0(
    "^(",
    paste(valid_fid, collapse = "|"),
    ")(?=_)"
  )
  
  txt_page_work |>
    dplyr::mutate(
      
      fid = stringr::str_extract(
        id,
        fid_pattern
      ),
      
      city_id = sub(
        "_.*$",
        "",
        fid
      )
    )
}

#' Title : Ajouter un citycode corrigé
#'
#' Crée citycode_clean à partir de citycode.
#' Le citycode original est conservé.
#' Corrige le cas de Melbourne aux États-Unis.
#'
#' @param txt_page_work Table des pages web.
#'
#' @return Table avec une colonne citycode_clean.
#' @export
create_clean_citycode <- function(txt_page_work) {
  
  txt_page_work |>
    dplyr::mutate(
      citycode_clean = dplyr::case_when(
        
        urban_aggl == "Melbourne" &
          country_en == "United States of America" ~ "1556721_23062",
        
        TRUE ~ as.character(citycode)
      )
    )
}



#' Title : Ajouter les coordonnées lon et lat à txt_page_work
#'
#' Ajoute les coordonnées de référence de list_city_river
#' à txt_page_work à partir du fid.
#' Les coordonnées originales latitude et longitude sont conservées.
#' 
#' Pour Windsor et Détroit, qui appartiennent à la même
#' agglomération, un point intermédiaire commun est calculé.
#'
#' lon correspond à la longitude.
#' lat correspond à la latitude.
#'
#' @param txt_page_work Table des pages enrichie avec fid.
#' @param list_city_river Table de référence des paires ville-rivière.
#'
#' @return Table txt_page_work enrichie avec long et lat.
#' @export
#' @examples long= ...lat=..

add_page_coordinates <- function(txt_page_work, list_city_river) {
  
  # Coordonnées correspondant à chaque fid
  coordinates <- list_city_river |>
    dplyr::select(
      fid,
      lon = longitude,
      lat = latitude
    ) |>
    dplyr::distinct()
  
  
  # Point intermédiaire entre Windsor et Détroit
  windsor_detroit <- list_city_river |>
    dplyr::filter(
      urban_aggl %in% c("Windsor", "Détroit")
    ) |>
    dplyr::summarise(
      lon_intermediaire = mean(longitude, na.rm = TRUE),
      lat_intermediaire = mean(latitude, na.rm = TRUE)
    )
  
  
  # Ajout des coordonnées
  txt_page_work |>
    dplyr::left_join(
      coordinates,
      by = "fid"
    ) |>
    dplyr::mutate(
      
      lon = dplyr::if_else(
        urban_aggl %in% c("Windsor", "Détroit"),
        windsor_detroit$lon_intermediaire,
        lon
      ),
      
      lat = dplyr::if_else(
        urban_aggl %in% c("Windsor", "Détroit"),
        windsor_detroit$lat_intermediaire,
        lat
      )
    )
}
