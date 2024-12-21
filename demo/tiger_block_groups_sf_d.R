library(httr)
library(sf)
library(here)
library(usmap)
library(ggplot2)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

# Get block groups for Washington District of Columbia
dc_fips <- usmap::fips(state = "dc")
dc_block_groups_sf <- RcensusPkg::tiger_block_groups_sf(
  state = dc_fips,
  output_dir = output_dir
)
# Map the sf
dc_block_groups_plot <- RspatialPkg::get_geom_sf(sf = dc_block_groups_sf)
dc_block_groups_plot

# Get a generalized version of the geometries
dc_block_groups_general_sf <- RcensusPkg::tiger_block_groups_sf(
  state = dc_fips,
  general = TRUE,
  output_dir = output_dir
)
# Map the sf
RspatialPkg::get_geom_sf(sf = dc_block_groups_general_sf)
