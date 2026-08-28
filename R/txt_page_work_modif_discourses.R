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
