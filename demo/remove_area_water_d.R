library(httr)
library(sf)
library(here)
library(usmap)
library(purrr)
library(ggplot2)
library(RspatialPkg)
library(RcensusPkg)

# Get the New York county tract shapefiles
ny_state_county_fips <- usmap::fips(state = "New York", county = "New York")
ny_state_fips <- substr(ny_state_county_fips, 1,2)
ny_county_fips <- substr(ny_state_county_fips, 3, 5)
express <- expression(COUNTYFP == ny_county_fips)
output_dir <- file.path(here(), "demo", "shapefiles")

ny_tracts_sf <- RcensusPkg::tiger_tracts_sf(
  state = ny_state_fips,
  vintage = 2020,
  general = TRUE,
  express = express,
  sf_info = FALSE,
  output_dir = output_dir
)

# Map the ny tracts
ny_tracts_plot <- RspatialPkg::get_geom_sf(
  sf = ny_tracts_sf,
  sf_fill = "green",
  hide_x_tics = T,
  hide_y_tics = T
)

ny_tracts_without_water_sf <- RcensusPkg::remove_area_water(ny_tracts_sf, output_dir = output_dir)

ny_tracts_without_water_plot <- RspatialPkg::get_geom_sf(
  sf = ny_tracts_without_water_sf,
  sf_fill = "blue",
  hide_x_tics = T,
  hide_y_tics = T
)

layout <- list(
  plots = list(ny_tracts_plot, ny_tracts_without_water_plot),
  rows = c(1, 1),
  cols = c(1, 2)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  title = "New York with/without Water",
  plot_titles = c("With water","Without water")
)
