library(httr)
library(sf)
library(here)
library(usmap)
library(ggplot2)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

# Get simple feature for us primary roads
us_roads_sf <- RcensusPkg::tiger_roads_sf(output_dir = output_dir)
# Map the geometries of us roads
# With over 17000 observations, the mapping is time consuming
us_roads_plot <- RspatialPkg::get_geom_sf(
  sf = us_roads_sf,
  hide_x_tics = TRUE,
  hide_y_tics = TRUE
)
us_roads_plot

# Get simple feature for Ohio roads
oh_fips <- usmap::fips(state = "ohio")
ohio_roads_sf <- RcensusPkg::tiger_roads_sf(
  state = oh_fips,
  entity = "state_roads",
  output_dir = output_dir
)
# Map the simple feature
ohio_roads_plot <- RspatialPkg::get_geom_sf(sf = ohio_roads_sf)
ohio_roads_plot

# Get simple feature for Geauga County, Ohio
oh_geauga_fips <- usmap::fips(state = "ohio", county = "geauga")
oh_fips <- substr(oh_geauga_fips, 1,2)
geauga_fips <- substr(oh_geauga_fips, 3, 5)
geauga_roads_sf <- RcensusPkg::tiger_roads_sf(
  state = oh_fips,
  county = geauga_fips,
  entity = "county_roads",
  output_dir = output_dir
)
# Map the simple feature
geauga_roads_plot <- RspatialPkg::get_geom_sf(
  sf = geauga_roads_sf,
  hide_x_tics = TRUE,
  hide_y_tics = TRUE
)
geauga_roads_plot
