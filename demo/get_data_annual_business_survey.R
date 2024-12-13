library(data.table)
library(magrittr)
library(httr)
library(here)
library(ggplot2)
library(usmap)
library(stringr)
library(RspatialPkg)
library(RcensusPkg)

# Support
output_dir <- file.path(here(), "demo", "shapefiles")

# ------------------Annual Business Survey (ABS) 2019, 2020, 2021----------------
# Description of the Annual Business Survey: https://www.census.gov/data/developers/data-sets/abs.html

# ------------------------Company Summary-------------------------------
# Get the available geographies for dataset "abscs" Company Summary:
abscs_geographies_2020_dt <- RcensusPkg::get_geography(dataset = "abscs", vintage = 2020)

# Get the NAICS2017 code and label ("NAICS2017", "NAICS2017_LABEL"), "Number of employer firms" ("FIRMPDEMP"),
#   "Number of employees" ("EMP"), "Annual payroll ($1,000)" ("PAYANN") for "Real Estate and rental and leasing"
#   (NAICS2017 == 53) across the metropolitan statistical area/micropolitan statistical areas for 2020.
real_estate_metro_dt <- RcensusPkg::get_vintage_data(
  dataset = "abscs",
  vintage = 2020,
  vars = c("NAICS2017", "NAICS2017_LABEL", "FIRMPDEMP", "EMP", "PAYANN"),
  predicates = "&NAICS2017=53",
  region = "metropolitan statistical area/micropolitan statistical area:*"
) %>%
  .[, .(NAME = stringr::str_remove(NAME, " Metro Area"), GEOID, NAICS2017 = as.integer(NAICS2017), FIRMPDEMP = as.integer(FIRMPDEMP), EMP = as.integer(EMP), PAYANN = as.integer(PAYANN), NAICS2017_LABEL)] %>%
  data.table::setorderv(cols = "PAYANN", order = -1)

# Get the geometries of the top 10 metro areas based on real estate company annual payroll.
cbsa_sf <- RcensusPkg::tiger_cbsa_sf(
  output_dir = output_dir,
  vintage = 2020,
  general = TRUE,
  sf_info = FALSE,
  datafile = real_estate_metro_dt[1:10,],
  datafile_key = "GEOID",
  sf_key = "GEOID",
  check_na = TRUE
)

# Map the locations of the top 15 metro areas
real_estate_metro_plot <- RspatialPkg::get_geom_sf(
  sf = cbsa_sf,
  aes_text = "i.NAME",
  hide_x_tics = TRUE,
  hide_y_tics = TRUE
)
real_estate_metro_plot
