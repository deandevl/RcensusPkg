library(httr)
library(sf)
library(usmap)
library(here)
library(ggplot2)
library(data.table)
library(magrittr)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

# Get the general state geometries for just Kentucky
# Create a logical expression that filters out Kentucky
kentucky_express <- expression(STATEFP == "21")
# Get the Kentucky geometries
kentucky_sf <- RcensusPkg::tiger_states_sf(
  general = TRUE,
  express = kentucky_express,
  output_dir = output_dir
)
# Create a plot object from kentucky_sf
kentucky_plot <- RspatialPkg::get_geom_sf(sf = kentucky_sf)

# Get the fips codes for Kentucky and its county Nelson County
kentucky_nelson_fips <- usmap::fips(state = "kentucky", county = "nelson")
kentucky_fips <- substr(kentucky_nelson_fips, 1,2)
nelson_fips <- substr(kentucky_nelson_fips, 3, 5)

# Get all the landmark geometries (i.e. points) for Kentucky
kentucky_landmarks_sf <- RcensusPkg::tiger_landmarks_sf(
  state = kentucky_fips,
  entity = "point",
  check_na = TRUE,
  output_dir = output_dir
)
#kentucky_landmarks_sf <- na.omit(kentucky_landmarks_sf)

# Map the Kentucky landmark geometries over the geometries for Kentucky
kentucky_landmarks_plot <-
  kentucky_plot %>%
  RspatialPkg::get_geom_sf(
    sf = kentucky_landmarks_sf
  )
kentucky_landmarks_plot

# Get the landmarks for just Nelson County, Kentucky by
#  intersecting all Kentucky landsmarks (i.e. kentucky_landmarks_sf)
#  geometries with the geometries for Nelson County

# Get the geometries for Nelson County
# Create an expression for getting the geometries for just Nelson County
nelson_express <- expression(STATEFP == "21" & COUNTYFP == "179")
# Get the geometries for just Nelson County
nelson_sf <- RcensusPkg::tiger_counties_sf(
  general = TRUE,
  express = nelson_express,
  output_dir = output_dir
)

# Intersect all the landmarks geometries (i.e. kentucky_landmarks_sf) to
# the Nelson County geometries (i.e. nelson_sf)
# We will get sf with just the landmark geometries of Nelson County geometries
# We have 78 landmarks in Nelson County
nelson_landmarks_intersect_lg <- sf::st_intersects(kentucky_landmarks_sf, nelson_sf, sparse = F)
nelson_landmarks_sf <- kentucky_landmarks_sf[nelson_landmarks_intersect_lg[,1],]

# Plot the geometries of Nelson landmarks over the geometries of Nelson County
# Use the variable "FULLNAME" from nelson_landmarks_sf
nelson_landmarks_plot <-
  RspatialPkg::get_geom_sf(
    sf = nelson_sf,
    sf_fill = "yellow",
    sf_alpha = 0.7,
    hide_x_tics = TRUE,
    hide_y_tics = TRUE) %>%
  RspatialPkg::get_geom_sf(
    sf = nelson_landmarks_sf,
    aes_text = "FULLNAME",
    text_color = "black",
    text_size = 2.5,
    text_nudge_y = 0.007
  ) %>%
  RspatialPkg::get_geom_sf(
    sf = nelson_landmarks_sf,
    sf_fill = "red",
    sf_size = 1.0
  )
nelson_landmarks_plot

