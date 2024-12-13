library(httr)
library(sf)
library(here)
library(data.table)
library(magrittr)
library(usmap)
library(ggplot2)
library(RspatialPkg)
library(RcensusPkg)

# Get the fips codes for the state of Texas and its county Tarrant
tx_tarrant_fips <- usmap::fips(state = "texas", county = "tarrant")
tx_fips <- substr(tx_tarrant_fips, 1, 2)
tarrant_fips <- substr(tx_tarrant_fips, 3, 5)
output_dir <- file.path(here(), "demo", "shapefiles")
# Download a generalized version of the Texas tracts shapefile
#   and convert it to a special feature(sf) object.
# This particular shapefile has NA for its crs, so we will
#   transform it to crs NAD83 (i.e. crs_transform = 3426).
# We will also show a progress bar during the download of the shapefile.

tx_tracts_sf <-  RcensusPkg::tiger_tracts_sf(
  state = tx_fips,
  vintage = 1990,
  set_crs = 3486,
  general = TRUE,
  output_dir = output_dir,
  delete_files = FALSE,
  do_progress = TRUE
)

# Get just the tracts for Tarrant County, Texas
tarrant_cty_tracts_sf <- data.table::as.data.table(tx_tracts_sf) %>%
  .[CO == tarrant_fips, ] %>%
  sf::st_as_sf(.)

# Map the simple feature (sf) object for Tarrant County, Texas tracts
tarrant_cty_tracts_plot <- RspatialPkg::get_geom_sf(
  sf = tarrant_cty_tracts_sf,
  sf_fill = "green"
)
tarrant_cty_tracts_plot

# -----------------------Read and convert to a simple feature a downloaded shapefile of all Texas tracts---------
# Change the crs to 4326
downloaded_shapefile_path <- output_dir
tx_tracts_downloaded_sf <- RcensusPkg::tiger_tracts_sf(
  shapefile = downloaded_shapefile_path,
  set_crs = 4326
)

# Map the simple feature
tx_tracts_downloaded_plot <- RspatialPkg::get_geom_sf(
  sf = tx_tracts_downloaded_sf,
  sf_fill = "yellow"
)
tx_tracts_downloaded_plot
