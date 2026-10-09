# BUILD LANDSCAPE DATA
# GloUrbViz


# --------------------------------------------------
# CHARGEMENT PACKAGE
# --------------------------------------------------

devtools::load_all()


# --------------------------------------------------
# LECTURE
# --------------------------------------------------

IPD_city <- read.csv(
  "data-raw/landscape/IPD_city.csv"
)

pixel <- read.csv(
  "data-raw/landscape/pixel.csv"
)


# --------------------------------------------------
# CREATION - TABLE GLOBALE
# --------------------------------------------------

glourbviz_landscape_global <-
  create_landscape_global_view(
    IPD_city,
    pixel
  )


# --------------------------------------------------
# CREATION - TABLE DETAILLEE DES IMPACTS
# --------------------------------------------------

glourbviz_landscape_impacts <-
  create_landscape_impacts(
    IPD_city,
    pixel
  )


# --------------------------------------------------
# CONTROLE
# --------------------------------------------------

cat(
  "Nombre d'agglomérations dans la table globale :",
  nrow(glourbviz_landscape_global),
  "\n"
)

cat(
  "Nombre de lignes dans la table détaillée :",
  nrow(glourbviz_landscape_impacts),
  "\n"
)

print(
  glourbviz_landscape_global
)

print(
  glourbviz_landscape_impacts
)


# --------------------------------------------------
# SAUVEGARDE
# --------------------------------------------------

saveRDS(
  glourbviz_landscape_global,
  "data/glourbviz_landscape_global.rds"
)

saveRDS(
  glourbviz_landscape_impacts,
  "data/glourbviz_landscape_impacts.rds"
)


cat(
  "\nTraitement terminé et données sauvegardées.\n"
)