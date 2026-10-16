# BUILD LANDSCAPE DATA
# GloUrbViz



# chrgt pckage
devtools::load_all()



# lecture des tbales
IPD_city <- read.csv(
  "data-raw/landscape/IPD_city.csv"
)
pixel <- read.csv(
  "data-raw/landscape/pixel.csv"
)


# Nettoyage des donnees pixels
  # on garde le nombre de pixels
n_pixel_avant <- nrow(pixel)
# ok la on met la fonction en route 
pixel <- clean_landscape_pixels(pixel)


# Creation de la table globale (on retrouve le nombre de I,P,D et aussi les pixels transf)
glourbviz_landscape_global <-
  create_landscape_global_view(
    IPD_city,
    pixel
  )

# Creations de la tables w/ impacts, pressions et drivers (I,P,D)
glourbviz_landscape_impacts <-
  create_landscape_impacts(
    IPD_city,
    pixel
  )


# des controles
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
cat(
  "Nombre de lignes supprimées :",
  n_pixel_avant - nrow(pixel),
  "\n"
)

print(
  glourbviz_landscape_global
)
print(
  glourbviz_landscape_impacts
)



# SAUVEGARDE Des tables utils à cet onglet ...

saveRDS(
  glourbviz_landscape_global,
  "data/glourbviz_landscape_global.rds"
)
saveRDS(
  glourbviz_landscape_impacts,
  "data/glourbviz_landscape_impacts.rds"
)
saveRDS(
  pixel,
  "data/glourbviz_landscape_pixels.rds"
)

 ###
cat(
  "\nTraitement terminé et données sauvegardées : c'est okkkk \n"
)