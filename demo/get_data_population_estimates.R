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

# Map the degree of density across the states
RcensusPkg::plot_us_data(
  df = state_pop_density_dt,
  states_col = "NAME",
  value_col = "DENSITY_2021",
  output_dir = output_dir,
  scale_breaks = seq(0,1400,200),
  scale_limits = c(0,1400),
  scale_palette = "YlGn",
  scale_labels = seq(0,1400,200)
)

# Map just the northeast states
northeast_density_dt <- state_pop_density_dt[NAME %in% c("New Jersey","Maine","Vermont","New York","Connecticut","Massachusetts","Rhode Island","New Hampshire","Delaware"),]
RcensusPkg::plot_us_data(
  df = northeast_density_dt,
  states_col = "NAME",
  value_col = "DENSITY_2021",
  output_dir = output_dir,
  scale_breaks = seq(0,1400,200),
  scale_limits = c(0,1400),
  scale_palette = "YlGn",
  scale_labels = seq(0,1400,200),
  sf_color = "white",
  sf_linewidth = 1.0
) +
  theme(
    panel.background = element_rect(fill = "blue")

  )
