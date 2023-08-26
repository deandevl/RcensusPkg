library(httr)
library(sf)
library(here)
library(usmap)
library(RspatialPkg)
library(ggplot2)
library(RcensusPkg)

# -------Map the geometries for Ohio counties-----

# Determine the fips code for Ohio (returns "39")
ohio_fips <- usmap::fips(state = "ohio")

# Create an expression to filter out just Ohio counties
#  from the simple feature dataframe
express <- expression(STATEFP == "39")

# Get the Ohio county's generalized geometries
ohio_counties_sf <- RcensusPkg::tiger_counties_sf(
  general = TRUE,
  express = express
)

# Map the Ohio counties
ohio_counties_plot <- RspatialPkg::get_geom_sf(sf = ohio_counties_sf)
ohio_counties_plot

# Get just Geauga County in Ohio by using the "express" parameter
# We need the fips code for Geauga County
ohio_geauga_fips <- usmap::fips(state="ohio", county="geauga")
geauga_fips <- substr(ohio_geauga_fips, 3, 5)

# Create an expression for filtering out geometries for just Geauga County
express <- expression(STATEFP == "39" & COUNTYFP == "055")

# Get the Geauga County geometries
geauga_sf <- RcensusPkg::tiger_counties_sf(
  general = TRUE,
  express = express
)

# Map Geauga County geometries
geauga_plot <- RspatialPkg::get_geom_sf(sf = geauga_sf)
geauga_plot

