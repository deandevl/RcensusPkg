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

# Goal: Get the median age for all US states and join this data with
#  state boundaries. Map the data and boundaries.

# Get data for variable "B01002_001E" (median age) for all the states
states_median_age_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1",
  vintage = 2019,
  vars = "B01002_001E",
  region = "state"
)

# Rename the "B01002_001E" column
data.table::setnames(states_median_age_dt, old = "B01002_001E", new = "MedianAge")

# simple wrangling
# Convert "MedianAge" column to numeric
states_median_age_dt[, MedianAge := as.numeric(MedianAge)]

# Get the simple feature geometries for the states and join the "states_median_age_dt"
#  dataframe with the simple feature dataframe.
output_dir <- file.path(here(), "demos", "shapefiles")
states_joined_sf <- RcensusPkg::tiger_states_sf(
  output_dir = output_dir,
  datafile = states_median_age_dt,
  datafile_key = "GEOID"
)

# Map the simple feature
states_joined_plot <- RspatialPkg::get_geom_sf(
  sf = states_joined_sf,
  aes_fill = "MedianAge"
) +
ggplot2::scale_fill_gradientn(
  colors = RColorBrewer::brewer.pal(n = 9, name = "Oranges"),
  n.breaks = 8
) +
ggplot2::coord_sf(
  xlim = c(-179.0, -60.0),
  ylim = c(15.0, 72.0)
)
states_joined_plot
