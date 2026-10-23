#' @title Nettoyer les données de pixels Landscape
#'
#' Remplace les NA des catégories de pixels par 0,
#' recalcule les totaux manquants ou nuls
#' et supprime les doublons hors identifiant technique.
#'
#' @param pixel Table des données de pixels.
#' @return Table de pixels nettoyée.
#' @export
clean_landscape_pixels <- function(pixel) {
  pixel_clean <- pixel |>
    dplyr::mutate(
      dplyr::across(
        c(npos,nneg, nneu),
        ~ tidyr::replace_na(.x, 0)
      ),
      num_tot = dplyr::if_else(
        is.na(num_tot) | num_tot == 0,
        npos + nneg + nneu,
        num_tot
      ),
      # Recalcul uniquement les prop qui st en NA
      perc_pos = dplyr::if_else(
        is.na(perc_pos) & num_tot > 0,
        npos / num_tot,
        perc_pos
      ),
      perc_neg = dplyr::if_else(
        is.na(perc_neg) & num_tot > 0,
        nneg / num_tot,
        perc_neg
      ),
      perc_neu = dplyr::if_else(
        is.na(perc_neu) & num_tot > 0,
        nneu / num_tot,
        perc_neu
      ),
      # Compléter la classification pour les zones à 100 % de pixels positifs
      categ_prop = dplyr::if_else(
        perc_pos == 1 & is.na(categ_prop),
        "A : Major increase and minor decrease",
        categ_prop
      ),
      code_prop = dplyr::if_else(
        perc_pos == 1 & is.na(code_prop),
        "A",
        code_prop
      )
    ) |>
    dplyr::distinct(
      dplyr::across(c(-X, -reach, -CityLength_km)),
      .keep_all = TRUE
    )
  return(pixel_clean)
}



#' @title Créer les indicateurs globaux de Landscape changes
#'
#' Calcule, pour chaque agglomération urbaine, le nombre de valeurs
#' distinctes d'Impact, Pressure et Driver à l'échelle de la ville,
#' du corridor et de la plaine.
#'
#' Les modalités "unchanged" et "no_data" sont conservées dans les
#' données détaillées mais ne sont pas comptées comme des impacts
#' ou des pressures.
#'
#' Les cellules contenant plusieurs Pressure ou plusieurs Driver
#' séparés par une virgule sont séparées temporairement avant le comptage.
#'
#' Calcule également la proportion de pixels ayant changé de
#' catégorie (pixels positifs et négatifs) à l'échelle de
#' l'agglomération entière, du corridor et de la plaine.
#' Conserve les coordonnées de chaque agglomération
#'
#' @param IPD_city Table contenant les informations Impact, Pressure
#'   et Driver par agglomération et par zone.
#'
#' @param pixel Table contenant les nombres de pixels positifs,
#'   négatifs et neutres ainsi que les coordonnées des agglomérations.
#'
#' @return Table contenant une ligne par agglomération urbaine.
#' @export
create_landscape_global_view <- function(IPD_city, pixel) {
  # aggl
  cities <- IPD_city |>
    dplyr::select(
      citycode,
      Urban_aggl
    ) |>
    dplyr::distinct()
  
  #IMPACTS (car separé par des virgules)
  IPD_impact <- IPD_city |>
    dplyr::mutate(
      Impact = stringr::str_trim(Impact),
      Impact = stringr::str_to_lower(Impact)
    ) |>
    dplyr::filter(
      !is.na(Impact),
      !Impact %in% c("no_data", "unchanged")
    )
    #PRESSURES
  IPD_pressure <- IPD_city |>
    tidyr::separate_rows(
      Pressure,
      sep = ","
    ) |>
    dplyr::mutate(
      Pressure = stringr::str_trim(Pressure),
      Pressure = stringr::str_to_lower(Pressure)
    ) |>
    dplyr::filter(
      !is.na(Pressure),
      !Pressure %in% c("no_data", "unchanged")
    )
  # DRIVERS
  IPD_driver <- IPD_city |>
    tidyr::separate_rows(
      Driver,
      sep = ","
    ) |>
    dplyr::mutate(
      Driver = stringr::str_trim(Driver),
      Driver = stringr::str_to_lower(Driver)
    ) |>
    dplyr::filter(
      !is.na(Driver)
    )
  
  #IMPACTS toute l'agglo
  impact_tot <- IPD_impact |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_impact_tot = dplyr::n_distinct(Impact),
      .groups = "drop"
    )
  # PRESSURES toute l'agglo
  pressure_tot <- IPD_pressure |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_pressure_tot = dplyr::n_distinct(Pressure),
      .groups = "drop"
    )
  #DRIVERS toute l'agglo
  driver_tot <- IPD_driver |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_driver_tot = dplyr::n_distinct(Driver),
      .groups = "drop"
    )
  
  #IMPACTS dans le corr
  impact_cor <- IPD_impact |>
    dplyr::filter(zone == "corridor") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_impact_cor = dplyr::n_distinct(Impact),
      .groups = "drop"
    )
  # PRESSURES dans le corridor
  pressure_cor <- IPD_pressure |>
    dplyr::filter(zone == "corridor") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_pressure_cor = dplyr::n_distinct(Pressure),
      .groups = "drop"
    )
  # DRIVERS dans le coori
  driver_cor <- IPD_driver |>
    dplyr::filter(zone == "corridor") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_driver_cor = dplyr::n_distinct(Driver),
      .groups = "drop"
    )
  
  #Plaine
  #IMPACTS
  impact_pl <- IPD_impact |>
    dplyr::filter(zone == "plain") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_impact_pl = dplyr::n_distinct(Impact),
      .groups = "drop"
    )
  #PRESSURES
  pressure_pl <- IPD_pressure |>
    dplyr::filter(zone == "plain") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_pressure_pl = dplyr::n_distinct(Pressure),
      .groups = "drop"
    )
  #DRIVERS
  driver_pl <- IPD_driver |>
    dplyr::filter(zone == "plain") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_driver_pl = dplyr::n_distinct(Driver),
      .groups = "drop"
    )
  
  # on rassemble tout
  IPD_global <- cities |>
    dplyr::left_join(impact_tot, by = "citycode") |>
    dplyr::left_join(pressure_tot, by = "citycode") |>
    dplyr::left_join(driver_tot, by = "citycode") |>
    dplyr::left_join(impact_cor, by = "citycode") |>
    dplyr::left_join(pressure_cor, by = "citycode") |>
    dplyr::left_join(driver_cor, by = "citycode") |>
    dplyr::left_join(impact_pl, by = "citycode") |>
    dplyr::left_join(pressure_pl, by = "citycode") |>
    dplyr::left_join(driver_pl, by = "citycode") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("n_"),
        ~ tidyr::replace_na(.x, 0L)
      )
    )
  
  # ok maintenant on s'interesse aux pixels qui ont changé : 
  #PIXELS agglo
  pixel_tot <- pixel |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      Latitude = dplyr::first(Latitude),
      Longitude = dplyr::first(Longitude),
      nchange_tot = sum(npos, nneg, na.rm = TRUE),
      num_tot_tot = sum(num_tot, na.rm = TRUE),
      perc_change_tot = dplyr::if_else(
        num_tot_tot > 0,
        nchange_tot / num_tot_tot * 100,
        NA_real_
      ),
      .groups = "drop"
    )
  # PIXELS corridor
  pixel_cor <- pixel |>
    dplyr::filter(zone == "corridor") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      nchange_cor = sum(npos, nneg, na.rm = TRUE),
      num_tot_cor = sum(num_tot, na.rm = TRUE),
      perc_change_cor = dplyr::if_else(
        num_tot_cor > 0,
        nchange_cor / num_tot_cor * 100,
        NA_real_
      ),
      .groups = "drop"
    )
  # PIXELS plaine
  pixel_pl <- pixel |>
    dplyr::filter(zone == "plain") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      nchange_pl = sum(npos, nneg, na.rm = TRUE),
      num_tot_pl = sum(num_tot, na.rm = TRUE),
      perc_change_pl = dplyr::if_else(
        num_tot_pl > 0,
        nchange_pl / num_tot_pl * 100,
        NA_real_
      ),
      .groups = "drop"
    )
  
  # assemblable pix et coordonnées
  pixel_city <- pixel_tot |>
    dplyr::left_join(
      pixel_cor,
      by = "citycode"
    ) |>
    dplyr::left_join(
      pixel_pl,
      by = "citycode"
    )
  
  # TABLE FINALE
  landscape_global <- IPD_global |>
    dplyr::left_join(
      pixel_city,
      by = "citycode"
    )
  return(landscape_global)
}



#' @title Créer la table détaillée des Landscape changes
#'
#' Conserve les informations détaillées relatives aux impacts,
#' pressures et drivers pour chaque agglomération et chaque zone.
#'
#' Les modalités "unchanged" et "no_data" sont conservées.
#'
#' Les espaces parasites et différences de casse sont corrigés.
#' Les valeurs multiples de Pressure et Driver restent dans leur
#' cellule d'origine afin de préserver les relations entre variables.
#'
#' Les coordonnées des agglomérations sont ajoutées depuis la table pixel.
#'
#' @param IPD_city Table détaillée contenant les informations relatives
#'   aux impacts, pressures et drivers.
#'
#' @param pixel Table contenant les coordonnées des agglomérations.
#'
#' @return Table détaillée des Landscape changes avec Latitude et Longitude.
#' @export
create_landscape_impacts <- function(IPD_city, pixel) {
  # nattoyage
  IPD_clean <- IPD_city |>
    dplyr::mutate(
      Impact = stringr::str_to_lower(
        stringr::str_trim(Impact)
      ),
      Pressure = stringr::str_to_lower(
        stringr::str_trim(Pressure)
      ),
      Driver = stringr::str_to_lower(
        stringr::str_trim(Driver)
      )
    )
  
  # Il manquait les coordonnées pour la table codage ... 
  city_coord <- pixel |>
    dplyr::select(
      citycode,
      Latitude,
      Longitude
    ) |>
    dplyr::distinct()
  
  landscape_impacts <- IPD_clean |>
    dplyr::left_join(
      city_coord,
      by = "citycode"
    )
  return(landscape_impacts)
}