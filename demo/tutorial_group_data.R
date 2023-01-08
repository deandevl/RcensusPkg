library(httr)
library(jsonlite)
library(data.table)
library(purrr)
library(RcensusPkg)

# The following R script shows how to obtain Census Bureau using
# a group acronym.

# Goal: Get all the variables under the group "B01001" in the "acs/acs5" dataset
# The group (sometimes referred to as table) "B01001" covers all the variables
#   for sex broken down by age.

B01001_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs5",
  vintage = 2020,
  group = "B01001",
  region = "state"
)


