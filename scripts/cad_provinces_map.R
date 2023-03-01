library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

cad_map <- 
  st_read('~/Downloads/lpr_000b16a_e/lpr_000b16a_e.shp') %>%
  mutate(
    PRENAME = ifelse(PRENAME %in% c("Yukon", "Northwest Territories", "Nunavut"), "Northern Canada", PRENAME)
  ) %>% 
  group_by(PRENAME) %>% 
  summarise(
    geometry = st_union(geometry)
  ) %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

readr::write_rds(cad_map, 'data/cad_provinces_map.rds')

leaflet(cad_map) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(color = "green", label = ~PRENAME)

# Bla bla bla map of provinces
# 
# ::: {.panel-tabset}
# 
# ### 2001
# 
# ```{r map_provinces_2001, echo = FALSE, warning = FALSE, message = FALSE}
# canadian_map_by_year(census_data_analysis, cad_map, "2001")
# ```
# 
# ### 2006
# 
# ```{r map_provinces_2006, echo = FALSE, warning = FALSE, message = FALSE}
# canadian_map_by_year(census_data_analysis, cad_map, "2006")
# ```
# 
# ### 2011
# 
# ```{r map_provinces_2011, echo = FALSE, warning = FALSE, message = FALSE}
# canadian_map_by_year(census_data_analysis, cad_map, "2011")
# ```
# 
# ### 2016
# 
# ```{r map_provinces_2016, echo = FALSE, warning = FALSE, message = FALSE}
# canadian_map_by_year(census_data_analysis, cad_map, "2016")
# ```
# 
# :::