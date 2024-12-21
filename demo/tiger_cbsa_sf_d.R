library(httr)
library(sf)
library(here)
library(usmap)
library(ggplot2)
library(data.table)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

# Get shapefile geometries for the Core-based statistical areas
#   of Texas with vintage 2020.
cbsa_tx_sf <- RcensusPkg::tiger_cbsa_sf(
  vintage = 2020,
  resol = "20m",
  #city_filter = "Kansas City",
  state_filter = "TX",
  general = TRUE,
  sf_info = FALSE,
  output_dir = output_dir
)
head(cbsa_tx_sf, 1)

# Map the cbsa boundaries for Kansas City
RspatialPkg::get_geom_sf(
  sf = cbsa_tx_sf,
  hide_x_tics = TRUE,
  hide_y_tics = TRUE
)


