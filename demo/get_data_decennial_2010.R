library(data.table)
library(ggplot2)
library(here)
library(usmap)
library(mapview)
library(RplotterPkg)
library(RspatialPkg)
library(RcensusPkg)

# -----------2010 Decennial data--------------------
# Description of Summary Files 1,2: https://www.census.gov/data/developers/data-sets/decennial-census.2010.html#list-tab-533552149

# From the 2010 Decennial Summary File 1 ("dec/sf1") dataset get metadata of the variable group "TOTAL POPULATION IN OCCUPIED HOUSING UNITS
#  BY TENURE (WHITE ALONE HOUSEHOLDER)" ("H11A").
white_house_ownership_vars_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf1",
  vintage = 2010,
  group = "H11A"
) |>
 _[, .(name, label = stringr::str_remove_all(label, "Population in occupied housing units!!"), predicateType)]

labels <- white_house_ownership_vars_dt$label[2:5]

# Get count estimates for the white tenure of home ownership across the US for 2010.
white_ownership_2010_dt <- get_vintage_data(
  dataset = "dec/sf1",
  vintage = 2010,
  group = "H11A",
  region = "us:1",
  wide_to_long = TRUE
) |>
  _[1:4,] |>
  _[, value := as.numeric(value)] |>
  _[, `:=`(percent = round(value/value[[1]] * 100, digits = 1), race = "white", label = factor(labels, levels = labels))] |>
  _[2:4,]


# Similarly get count estimates for the black tenure of home ownership
black_ownership_2010_dt <- get_vintage_data(
  dataset = "dec/sf1",
  vintage = 2010,
  group = "H11B",
  region = "us:1",
  wide_to_long = TRUE
) %>%
  .[1:4,] %>%
  .[, value := as.numeric(value)] %>%
  .[, `:=`(percent = round(value/value[[1]] * 100, digits = 1), race = "black", label = factor(labels, levels = labels))] %>%
  .[2:4,]


# Compare white and black tenure ownership in a bar chart
white_black_ownership_dt <- rbind(white_ownership_2010_dt, black_ownership_2010_dt)
RplotterPkg::create_bar_plot(
  df = white_black_ownership_dt,
  aes_x = "label",
  aes_y = "percent",
  aes_fill = "race",
  position = "dodge",
  x_title = "percent",
  rot_y_tic_label = TRUE,
  do_coord_flip = TRUE
) + ggplot2::scale_fill_manual(values = c("black","white"))

#---------------------------------
# From the 2010 Decennial Summary File 2 ("dec/sf2") dataset get metadata of the variable giving the total number of
#  grandchildren under 18 years of age, living with grandparents ("PCT035001")
grandchildren_variable_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf2",
  vintage = 2010,
  vars = "PCT035001"
)

# -----------------------------------
# Get the number of grandchildren living with grandparents by tract in Cuyahoga County, Ohio.
cuyahoga_ohio <- usmap::fips(state = "Ohio", county = "Cuyahoga")
ohio_fips <- substr(cuyahoga_ohio, 1, 2)
cuyahoga_fips <- substr(cuyahoga_ohio, 3, 5)

vars_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf2",
  vintage = 2010
)

cuyahoga_ohio_grandchildren_dt <- RcensusPkg::get_vintage_data(
  dataset = "dec/sf2",
  vintage = 2010,
  vars = "PCT035001",
  region = "tract",
  regionin = paste0("state:", ohio_fips)
) |>
  _[county == cuyahoga_fips,] |>
  data.table::setnames(old = "PCT035001", new = "total_grandchildren") |>
  _[, .(GEOID, tract, total_grandchildren = as.numeric(total_grandchildren))]

# Create a simple features data.frame (sf) that incorporates the above data.table with the "total_grandchildren" living
#  living with grandparents.
output_dir <- file.path(here(), "demo", "shapefiles")
express <- expression(COUNTYFP10 == cuyahoga_fips)
cuyahoga_ohio_tracts_grandchildren_sf <- RcensusPkg::tiger_tracts_sf(
  state = ohio_fips,
  output_dir = output_dir,
  vintage = 2010,
  datafile = cuyahoga_ohio_grandchildren_dt,
  datafile_key = "GEOID",
  sf_key = "GEOID10",
  express = express,
  sf_info = FALSE
)

# Plot the simple feature showing the county's tracts and their respective "total_grandchildren" under 18
#   living with grandparents.
#  Static plot with ggplot2
RspatialPkg::get_geom_sf(
  sf = cuyahoga_ohio_tracts_grandchildren_sf,
  aes_fill = "total_grandchildren",
  hide_x_tics = FALSE,
  hide_y_tics = FALSE
) + ggplot2::coord_sf(ylim = c(41.26,41.615))

mapview::mapview(cuyahoga_ohio_tracts_grandchildren_sf, zcol = "total_grandchildren")
