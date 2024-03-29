library(httr)
library(sf)
library(usmap)
library(here)
library(ggplot2)
library(RspatialPkg)
library(RcensusPkg)

# Warning: This is a lengthy download and mapping
output_dir <- file.path(here(), "demo", "shapefiles")

nm_fips <- usmap::fips(state = "new mexico")
nm_blocks_sf <- RcensusPkg::tiger_blocks_sf(
  state = nm_fips,
  output_dir = output_dir
)
# Map the sf
nm_blocks_plot <- RspatialPkg::get_geom_sf(sf = nm_blocks_sf)
nm_blocks_plot
