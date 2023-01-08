library(jsonlite)
library(data.table)
library(httr)
library(RcensusPkg)

# Get the geographies available for dataset "acs/acs1/profile" with vintage 2019
acs1_profile_geo_dt <- RcensusPkg::get_geography(
  dataset = "acs/acs1/profile",
  vintage = 2019
)

