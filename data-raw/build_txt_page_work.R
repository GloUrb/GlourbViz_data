# BUILD TXT_PAGE_WORK

devtools::load_all()

txt_page_work <- readRDS(
  "data-raw/discourses/txt_page_work.rds"
)

list_city_river <- read.csv(
  "data-raw/discourses/list_city_river.csv"
)

txt_page_work <- create_page_ids(
  txt_page_work,
  list_city_river
)

saveRDS(
  txt_page_work,
  "data/glourbviz_txt_page_work.rds"
)