library(httr)
library(sf)
library(data.table)
library(magrittr)
library(usmap)
library(here)
library(RspatialPkg)
library(ggplot2)
library(RcensusPkg)

# Get the generalized tract geometries for Los Alamos, New Mexico
# Determine the fips codes for New Mexico and Los Alamos
nm_los_alamos_fips <- usmap::fips(state = "new mexico", county = "los alamos")
nm_fips <- substr(nm_los_alamos_fips, 1, 2)
los_alamos_fips <- substr(nm_los_alamos_fips, 3, 5)

# Create an expression to filter just the Los Alamos geometries
express <- expression(COUNTYFP == "028")

# Get the tract geometries for just Los Alamos
losalamos_tracts_sf <- RcensusPkg::tiger_tracts_sf(
  state = nm_fips,
  general = TRUE,
  express = express
)

# Map the Los Alamos County tract geometries
losalamos_tracts_plot <- RspatialPkg::get_geom_sf(sf = losalamos_tracts_sf)
losalamos_tracts_plot
