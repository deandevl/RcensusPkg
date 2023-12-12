library(data.table)
library(magrittr)
library(here)
library(sf)
library(stringr)
library(usmap)
library(RspatialPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")
ohio_hc_fips <- usmap::fips(state = "ohio", county = "holmes")
ohio_fips <- substr(ohio_hc_fips,1,2)
hc_fips <- substr(ohio_hc_fips,3,5)
express <- expression(COUNTYFP == hc_fips)

hc_median_home_ctysub_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs5/profile",
  vintage = 2020,
  vars = "DP04_0089E",
  region = "county subdivision",
  regionin = paste0("state:", ohio_fips, "+county:", hc_fips)
) %>%
  data.table::setnames(old = "DP04_0089E", new = "median_home_value") %>%
  .[, median_home_value := as.numeric(median_home_value)] %>%
  .[, NAME := stringr::str_remove(NAME, pattern = "township, Holmes County, Ohio")]

hc_median_home_ctysub_sf <- RcensusPkg::tiger_county_subsection_sf(
  state = ohio_fips,
  output_dir = output_dir,
  vintage = 2020,
  general = T,
  sf_info = F,
  datafile = hc_median_home_ctysub_dt,
  datafile_key = "GEOID",
  express = express
)

RspatialPkg::get_geom_sf(
  sf = hc_median_home_ctysub_sf,
  aes_fill = "median_home_value",
  aes_text = "NAME",
  hide_x_tics = T,
  hide_y_tics = T
)
