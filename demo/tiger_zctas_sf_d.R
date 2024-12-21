library(httr)
library(sf)
library(here)
library(ggplot2)
library(data.table)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

# get ZCTA data near Boston, MA with the prefixes "021","022","024"
#   for the "ZCTA5CE20" column.

# selected municipalities in the county of Middlesex, MA near Boston
mun_zcta_dt <- data.table(
  name = c("Carlisle","Concord","Bedford","Lexington", "Lexington", "Lincoln","Lincoln","Sudbury","Wayland","Weston","Waltham"),
  zcta = c("01741","01742","01730","02420","02421","01773","01731","01776","01778","02493","02451")
)

express <- expression(ZCTA5CE20 %in% mun_zcta_dt$zcta)

mun_zcta_sf <- RcensusPkg::tiger_zctas_sf(
  vintage = 2020,
  general = TRUE,
  output_dir = output_dir,
  do_progress = TRUE,
  express = express
)

# Map the Boston area ZCTA
RspatialPkg::get_geom_sf(sf = mun_zcta_sf)

# get dataset names
datasets_dt <- RcensusPkg::get_dataset_names(
  vintage = 2021,
  filter_title_str = "detailed tables"
)

# select the "acs/acs5 and get a list of variables for 2021 related to "family income"

# get variables related to "family income"
vars_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs5",
  vintage = 2021,
  filter_label_str = "family income"
)

# variable of interest is "B19113_001E" --
#   "Estimate!!Median family income in the past 12 months (in 2021 inflation-adjusted dollars)"

# check geography for getting zip code tabulation area (zcta)
geo_dt <- RcensusPkg::get_geography(
  dataset = "acs/acs5",
  vintage = 2021
)

# get the data for the variable and all the zcta codes
zcta_str <- toString(mun_zcta_dt$zcta)
income_dt <- RcensusPkg::get_vintage_data(
  dataset =  "acs/acs5",
  vintage = 2021,
  vars = "B19113_001E",
  region = paste0("zip code tabulation area:", zcta_str)
)

# join the data to the sf tiger zcta geographies
mun_zcta_data_sf <- RcensusPkg::tiger_zctas_sf(
  vintage = 2020,
  general = TRUE,
  output_dir = output_dir,
  do_progress = TRUE,
  sf_info = TRUE,
  express = express,
  datafile = income_dt,
  datafile_key = "GEOID",
  sf_key = "GEOID20"
) |>
  data.table::as.data.table() |>
  data.table::setnames(old = "B19113_001E", new = "Family_Income") |>
  _[, .(GEOID, Family_Income = as.numeric(Family_Income), geometry)] |>
  sf::st_sf()

# map the data with tiger geographies
RspatialPkg::get_geom_sf(
  sf = mun_zcta_data_sf,
  aes_fill = "Family_Income",
  hide_x_tics = TRUE,
  hide_y_tics = TRUE,
  panel_color = "white",
  caption = "Income for families in the county of Middlesex, MA near Boston"
)
