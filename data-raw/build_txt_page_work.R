# BUILD TXT_PAGE_WORK

devtools::load_all()


#------# la data source
txt_page_work <- readRDS(
  "data-raw/discourses/txt_page_work.rds"
)

list_city_river <- read.csv(
  "data-raw/discourses/list_city_river.csv"
)
#------


# fid et city_id
txt_page_work <- create_page_ids(
  txt_page_work,
  list_city_river
)

# citycode_clean
txt_page_work <- create_clean_citycode(
  txt_page_work
)

#coord
txt_page_work <- add_page_coordinates(
  txt_page_work,
  list_city_river
)


# 3. Contrôles coord
sum(is.na(txt_page_work$lon))
sum(is.na(txt_page_work$lat))

range(txt_page_work$lon, na.rm = TRUE)
range(txt_page_work$lat, na.rm = TRUE)

#svg
saveRDS(
  txt_page_work,
  "data/glourbviz_txt_page_work.rds"
)