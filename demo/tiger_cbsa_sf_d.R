library(httr)
library(sf)
library(here)
library(usmap)
library(ggplot2)
library(data.table)
library(magrittr)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

# Get shapefile geometries for the Core-based statistical areas
#   of Kansas City with vintage 2020.
cbsa_tx_sf <- RcensusPkg::tiger_cbsa_sf(
  vintage = 2020,
  resol = "20m",
  #city_filter = "Kansas City",
  state_filter = "TX",
  output_dir = output_dir,
  general = TRUE,
  sf_info = FALSE
)
cbsa_tx_sf

# Map the cbsa boundaries for Kansas City
cbsa_tx_plot <- RspatialPkg::get_geom_sf(
  sf = cbsa_tx_sf,
  hide_x_tics = TRUE,
  hide_y_tics = TRUE
)
cbsa_tx_plot


