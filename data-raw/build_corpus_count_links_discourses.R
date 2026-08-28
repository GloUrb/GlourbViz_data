# BUILD CORPUS COUNT LINKS
# GloUrbViz


# CHARGEMENT PACKAGE

devtools::load_all()

# LECTURE

corpus <- read.csv(
  "data-raw/discourses/corpus.csv"
)

# CREATION 
glourbviz_corpus_count_links <-
  create_corpus_count_links(
    corpus
  )

# CONTROLE

cat(
  "Nombre de lignes :",
  nrow(glourbviz_corpus_count_links),
  "\n"
)

print(
  glourbviz_corpus_count_links
)


# Svg

saveRDS(
  glourbviz_corpus_count_links,
  "data/glourbviz_corpus_count_links.rds"
)

cat(
  "\nTraitement terminé et données sauvegardées.\n"
)