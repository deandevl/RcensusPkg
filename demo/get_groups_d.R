library(jsonlite)
library(data.table)
library(httr)
library(RcensusPkg)

acs1_profile_groups_dt <- RcensusPkg::get_groups(
  dataset = "acs/acs1/profile",
  vintage = 2019)


acs5_groups_dt <- RcensusPkg::get_groups(
  dataset = "acs/acs5",
  vintage = 2019)
