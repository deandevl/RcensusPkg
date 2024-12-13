library(httr)
library(sf)
library(here)
library(ggplot2)
library(data.table)
library(magrittr)
library(stringr)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

# Get the shapefile geometries for the Census Bureau's defined
#  "urban areas" across the US for 2019:

us_urban_areas_sf <- RcensusPkg::tiger_urban_area_sf(
  output_dir = output_dir,
  vintage = 2019,
  general = TRUE
)

# Get the populations of urbanized areas ("B01001_001E") from the
#  "acs/acs1" dataset in the US for 2019, Select those with populations
#  greater than 750000 and are in the state of California:
ca_urban_pop_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1",
  vintage = 2019,
  vars = "B01001_001E",
  regionin = "urban_area"
) %>%
  data.table::setnames(old = "B01001_001E",new = "population") %>%
  .[, population := as.numeric(population)] %>%
  .[population >= 750000 & grepl(pattern = "CA Urbanized Area (2010)", NAME, fixed = T),] %>%
  .[, NAME := stringr::str_remove(NAME, fixed(" Urbanized Area (2010)"))]

# Join us_urban_area_sf with ca_urban_pop_dt by NAME
ca_urban_pop_sf <- RcensusPkg::join_it(
  df_1 = ca_urban_pop_dt,
  df_2 = us_urban_areas_sf,
  key_1 = "NAME",
  key_2 = "NAME10",
  match = TRUE,
  return_sf = TRUE
) %>% data.table::as.data.table(.) %>%
  .[, NAME := stringr::str_remove(NAME, fixed(", CA"))] %>%
  .[, Label := paste(NAME,population)] %>%
  sf::st_as_sf(.)

# Map the state of California along with its major urban areas and their degree of population
express <- expression(STATEFP == "06")
ca_geo_sf <- RcensusPkg::tiger_states_sf(
  output_dir = output_dir,
  vintage = 2019,
  general = TRUE,
  express = express,
  sf_info = FALSE
)


RspatialPkg::get_geom_sf(
  sf = ca_urban_pop_sf,
  aes_text = "Label",
  hide_x_tics = T,
  hide_y_tics = T,
  text_nudge_x = 0.5,
  text_nudge_y = 0.4
) +
  RspatialPkg::get_geom_sf(
    sf = ca_geo_sf,
    sf_size = 3,
    sf_fill = "white",
    sf_alpha = 0.01,
    hide_x_tics = T,
    hide_y_tics = T,
    adding = T
  )
