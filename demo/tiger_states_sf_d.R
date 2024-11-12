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

# Get shapefile geometries for us states
states_sf <- RcensusPkg::tiger_states_sf(output_dir = output_dir)
states_sf

# Map the us states geometries
states_plot <- RspatialPkg::get_geom_sf(
  sf = states_sf
) +
ggplot2::coord_sf(
  xlim = c(-179.0, -60.0),
  ylim = c(15.0, 72.0)
)
states_plot

# Get the fips code for Florida
florida_fips <- usmap::fips(state = "florida")

# Filter out the geometries for Florida manually using data.table techniques
florida_sf <- data.table::as.data.table(states_sf) %>%
  .[STATEFP == florida_fips, ] %>%
  sf::st_as_sf(.)

# Map the geometries of Florida
florida_plot <- RspatialPkg::get_geom_sf(sf = florida_sf)
florida_plot

# Get a generalized version of Florida geometries where
#  this time we will filter using the "express" parameter.
# The fips code for Florida is the character string "12".
express <- expression(STATEFP =="12")
florida_general_sf <- RcensusPkg::tiger_states_sf(
  general = TRUE,
  output_dir = output_dir,
  express = express
)

# Again, map the generalized Florida geometries
florida_general_plot <- RspatialPkg::get_geom_sf(sf = florida_general_sf)
florida_general_plot
