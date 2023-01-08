library(data.table)
library(magrittr)
library(sf)
library(here)
library(usmap)
library(ggplot2)
library(RspatialPkg)
library(RcensusPkg)

# The following R script is a tutorial on joining Census Bureau data with
# with simple feature geometries.

# Goal: Get the median household income by tract for Washington DC and join
# this data with DC tract boundaries. Map the data and boundaries.

# Get data for variable "B19013_001E" (median household income)
#  for the District of Columbia

# Get the available 2020 geographies for dataset "acs/acs5"
acs5_geo_dt <- RcensusPkg::get_geography(
  dataset = "acs/acs5",
  vintage = 2020
)
# "tract" is one of the available geographies

# Get the 2020 median household income data by tract for DC
dc_fips <- usmap::fips(state = "dc")
dc_B19013_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs5",
  vintage = 2020,
  vars = "B19013_001E",
  region = "tract",
  regionin = paste0("state:", dc_fips)
)

# Rename column "B19013_001E"
data.table::setnames(dc_B19013_dt, old = "B19013_001E", new = "MedianIncome")

# simple wrangling
# Remove rows with value = -666666666
# Convert value column to numeric
dc_B19013_dt <- dc_B19013_dt[MedianIncome != -666666666, ] %>%
  .[, MedianIncome := as.numeric(MedianIncome)]

# Get the simple feature DC tract geometries and join the data dataframe "dc_B19013_dt"
output_dir <- file.path(here(), "demos", "shapefiles")
dc_tracts_sf <- RcensusPkg::tiger_tracts_sf(
  output_dir = output_dir,
  state = dc_fips
)

# Join the data with simple feature object
dc_joined_sf <- RcensusPkg::join_it(
  df_1 = dc_B19013_dt,
  df_2 = dc_tracts_sf,
  key_1 = "GEOID",
  key_2 = "GEOID",
  return_sf = T
)

# Map the joined simple feature geometries with "value" column as
#  a fill aesthetic variable.
dc_joined_plot <- RspatialPkg::get_geom_sf(
  sf = dc_joined_sf,
  aes_fill = "MedianIncome"
) +
ggplot2::scale_fill_gradientn(
  colors = RColorBrewer::brewer.pal(n = 9, name = "Greens"),
  n.breaks = 8
)
dc_joined_plot


# --------------------The following also works----------
# Create the simple feature with a joined data dataframe
dc_joined_alternative__sf <- RcensusPkg::tiger_tracts_sf(
  output_dir = output_dir,
  state = dc_fips,
  datafile = dc_B19013_dt,
  datafile_key = "GEOID"
)
# Map the simple feature
dc_joined_alternative_plot <- RspatialPkg::get_geom_sf(
  sf = dc_joined_alternative__sf,
  aes_fill = "MedianIncome"
)
dc_joined_alternative_plot
