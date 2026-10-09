#' Créer les indicateurs globaux de Landscape changes
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
  
  
  # --------------------------------------------------
  # LISTE DES AGGLOMERATIONS
  # --------------------------------------------------
  
  cities <- IPD_city |>
    dplyr::select(
      citycode,
      Urban_aggl
    ) |>
    dplyr::distinct()
  
  
  # --------------------------------------------------
  # PREPARATION DES IMPACTS
  # --------------------------------------------------
  
  IPD_impact <- IPD_city |>
    dplyr::mutate(
      Impact = stringr::str_trim(Impact),
      Impact = stringr::str_to_lower(Impact)
    ) |>
    dplyr::filter(
      !is.na(Impact),
      !Impact %in% c("no_data", "unchanged")
    )
  
  
  # --------------------------------------------------
  # PREPARATION DES PRESSURES
  # --------------------------------------------------
  
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
  
  
  # --------------------------------------------------
  # PREPARATION DES DRIVERS
  # --------------------------------------------------
  
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
  
  
  # --------------------------------------------------
  # IMPACTS - AGGLOMERATION ENTIERE
  # --------------------------------------------------
  
  impact_tot <- IPD_impact |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_impact_tot = dplyr::n_distinct(Impact),
      .groups = "drop"
    )
  
  
  # --------------------------------------------------
  # PRESSURES - AGGLOMERATION ENTIERE
  # --------------------------------------------------
  
  pressure_tot <- IPD_pressure |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_pressure_tot = dplyr::n_distinct(Pressure),
      .groups = "drop"
    )
  
  
  # --------------------------------------------------
  # DRIVERS - AGGLOMERATION ENTIERE
  # --------------------------------------------------
  
  driver_tot <- IPD_driver |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_driver_tot = dplyr::n_distinct(Driver),
      .groups = "drop"
    )
  
  
  # --------------------------------------------------
  # IMPACTS - CORRIDOR
  # --------------------------------------------------
  
  impact_cor <- IPD_impact |>
    dplyr::filter(zone == "corridor") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_impact_cor = dplyr::n_distinct(Impact),
      .groups = "drop"
    )
  
  
  # --------------------------------------------------
  # PRESSURES - CORRIDOR
  # --------------------------------------------------
  
  pressure_cor <- IPD_pressure |>
    dplyr::filter(zone == "corridor") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_pressure_cor = dplyr::n_distinct(Pressure),
      .groups = "drop"
    )
  
  
  # --------------------------------------------------
  # DRIVERS - CORRIDOR
  # --------------------------------------------------
  
  driver_cor <- IPD_driver |>
    dplyr::filter(zone == "corridor") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_driver_cor = dplyr::n_distinct(Driver),
      .groups = "drop"
    )
  
  
  # --------------------------------------------------
  # IMPACTS - PLAINE
  # --------------------------------------------------
  
  impact_pl <- IPD_impact |>
    dplyr::filter(zone == "plain") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_impact_pl = dplyr::n_distinct(Impact),
      .groups = "drop"
    )
  
  
  # --------------------------------------------------
  # PRESSURES - PLAINE
  # --------------------------------------------------
  
  pressure_pl <- IPD_pressure |>
    dplyr::filter(zone == "plain") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_pressure_pl = dplyr::n_distinct(Pressure),
      .groups = "drop"
    )
  
  
  # --------------------------------------------------
  # DRIVERS - PLAINE
  # --------------------------------------------------
  
  driver_pl <- IPD_driver |>
    dplyr::filter(zone == "plain") |>
    dplyr::group_by(citycode) |>
    dplyr::summarise(
      n_driver_pl = dplyr::n_distinct(Driver),
      .groups = "drop"
    )
  
  
  # --------------------------------------------------
  # ASSEMBLAGE DES INDICATEURS IPD
  # --------------------------------------------------
  
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
  
  

  
  # --------------------------------------------------
  # PIXELS - AGGLOMERATION ENTIERE
  # --------------------------------------------------
  
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
  
  
  # --------------------------------------------------
  # PIXELS - CORRIDOR
  # --------------------------------------------------
  
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
  
  
  # --------------------------------------------------
  # PIXELS - PLAINE
  # --------------------------------------------------
  
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
  
  
  # --------------------------------------------------
  # ASSEMBLAGE PIXELS + COORDONNEES
  # --------------------------------------------------
  
  pixel_city <- pixel_tot |>
    dplyr::left_join(
      pixel_cor,
      by = "citycode"
    ) |>
    dplyr::left_join(
      pixel_pl,
      by = "citycode"
    )
  
  
  # --------------------------------------------------
  # TABLE FINALE
  # --------------------------------------------------
  
  landscape_global <- IPD_global |>
    dplyr::left_join(
      pixel_city,
      by = "citycode"
    )
  
  
  return(landscape_global)
}



#' Créer la table détaillée des Landscape changes
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
  
  
  # --------------------------------------------------
  # NETTOYAGE DES VARIABLES
  # --------------------------------------------------
  
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
  
  
  # --------------------------------------------------
  # COORDONNEES PAR AGGLOMERATION
  # --------------------------------------------------
  
  city_coord <- pixel |>
    dplyr::select(
      citycode,
      Latitude,
      Longitude
    ) |>
    dplyr::distinct()
  
  
  # --------------------------------------------------
  # AJOUT DES COORDONNEES A LA TABLE DETAILLEE
  # --------------------------------------------------
  
  landscape_impacts <- IPD_clean |>
    dplyr::left_join(
      city_coord,
      by = "citycode"
    )
  
  
  return(landscape_impacts)
}