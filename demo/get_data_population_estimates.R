library(data.table)
library(magrittr)
library(httr)
library(ggplot2)
library(usmap)
library(here)
library(RspatialPkg)
library(RcensusPkg)

# Support data
output_dir <- file.path(here(), "demo", "shapefiles")

# Get the possible geographies for dataset "pep/population"
pep_pop_geos_dt <- RcensusPkg::get_geography(
  dataset = "pep/population",
  vintage = 2021
)
# We see that geographies go down to the "state" level

# Get the population density (persons per square mile) on July 1, 2021 for each state not
#   including DC (GEOID = 11) and Puerto Rico (GEOID = 72)
state_pop_density_dt <- RcensusPkg::get_vintage_data(
  dataset = "pep/population",
  vintage = 2021,
  vars = "DENSITY_2021",
  region = "state:*"
) %>%
  .[, .(NAME, GEOID, DENSITY_2021 = round(as.numeric(DENSITY_2021), digits = 0))] %>%
  .[GEOID != 11 & GEOID != 72,]

# Get the simple feature (sf) geometries for the states and join the above population density data
# Remove any NA values
states_densities_sf <- RcensusPkg::tiger_states_sf(
  output_dir = output_dir,
  vintage = 2021,
  general = T,
  sf_info = F,
  datafile = state_pop_density_dt,
  datafile_key = "GEOID",
  sf_key = "GEOID",
  check_na = T
)

# Map the state geometries and degree of population density
states_densities_plot <- RspatialPkg::get_geom_sf(
  sf = states_densities_sf,
  aes_fill = "DENSITY_2021",
  aes_text = "DENSITY_2021",
  sf_color = "white",
  text_color = "white",
  hide_x_tics = T,
  hide_y_tics = T
) +
ggplot2::coord_sf(
  xlim = c(-179.0, -60.0),
  ylim = c(15.0, 72.0)
)
states_densities_plot
