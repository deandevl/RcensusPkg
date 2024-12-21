library(httr)
library(sf)
library(here)
library(usmap)
library(ggplot2)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

state_county_fips <- usmap::fips(state = "Ohio", county = "Geauga")
state_fips <- substr(state_county_fips,1,2)
county_fips <- substr(state_county_fips,3,5)

# get area water
geauga_area_water_sf <- RcensusPkg::tiger_water_sf(
  state = state_fips,
  county = county_fips,
  output_dir = output_dir
)
head(geauga_area_water_sf,1)

RspatialPkg::get_geom_sf(
  sf = geauga_area_water_sf,
  sf_fill = "blue"
)

# get linear water
geauga_linear_water_sf <- RcensusPkg::tiger_water_sf(
  state = state_fips,
  county = county_fips,
  entity = "linear",
  output_dir = output_dir
)
head(geauga_linear_water_sf,1)

RspatialPkg::get_geom_sf(
  sf = geauga_linear_water_sf,
  sf_fill = "blue"
)

# get coastline
us_coastline_sf <- RcensusPkg::tiger_water_sf(
  entity = "coastline",
  output_dir = output_dir
)
head(us_coastline_sf,1)

RspatialPkg::get_geom_sf(
  sf = us_coastline_sf,
  sf_color = "brown"
)
