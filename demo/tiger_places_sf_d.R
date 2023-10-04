library(httr)
library(sf)
library(usmap)
library(here)
library(ggplot2)
library(data.table)
library(magrittr)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

kentucky_fips <- usmap::fips(state = "Kentucky")

# Filter out some major place geometries for Kentucky
major_places_express <- expression(NAME %in% c("Bardstown", "Bowling Green", "Louisville", "Versailles", "Owensboro", "Frankfort", "Elizabethtown", "Danville"))
kentucky_places_sf <- RcensusPkg::tiger_places_sf(
  state = kentucky_fips,
  express = major_places_express,
  general = TRUE,
  output_dir = output_dir
)

# Get Kentucky state boundary geometries
kentucky_express <- expression(STATEFP == "21")
kentucky_sf <- RcensusPkg::tiger_states_sf(
  general = TRUE,
  express = kentucky_express,
  output_dir = output_dir
)

# Map the major places and Kentucky state boundary geometries
kentucky_places_plot <- RspatialPkg::get_geom_sf(
  sf = kentucky_sf,
  sf_fill = "blue",
  sf_alpha = 0.7,
  panel_color = "brown",
  hide_x_tics = T,
  hide_y_tics = T
) +
RspatialPkg::get_geom_sf(
  sf = kentucky_places_sf,
  sf_fill = "yellow",
  adding = T
) +
RspatialPkg::get_geom_sf(
  sf = kentucky_places_sf,
  aes_text = "NAME",
  text_color = "white",
  text_size = 3,
  text_nudge_y = 0.1,
  adding = T
)
kentucky_places_plot
