library(jsonlite)
library(data.table)
library(httr)
library(usmap)
library(RcensusPkg)

# The following R script illustrates the general steps in obtaining
# US Census Bureau data.

# Goal: Get survey percentages on computer ownership and presence of
# broadband in Kentucky households.

# Check Annual Community Surveys for datasets and vintages available.
acs1_datasets_dt <- RcensusPkg::get_dataset_names(filter_name_str = "acs1")

# It appears the "acs/acs1/profile" (ACS 1-year Estimates Data Profiles) is the one we want
# and is available for 2021, our target vintage.

# Check for variable names. We're looking for estimates on computer ownership and broadband use.
# The vintage is 2021. Let's filter the "label" column of the resultant data.table for the string
# "computer".
acs1_variables <- RcensusPkg::get_variable_names(
  dataset = "acs/acs1/profile",
  vintage = 2021,
  filter_label_str = "computer"
)

# Looking over the 12 variable names we've found the following available:
# "DP02_0152E"  Estimate!!COMPUTERS AND INTERNET USE!!Total households
# "DP02_0153PE" Percent!!COMPUTERS AND INTERNET USE!!Total households!!With a computer
# "DP02_0154PE" Percent!!COMPUTERS AND INTERNET USE!!Total households!!With a broadband Internet subscription

# Variable names should always be checked because they change from year-to-year.

# Also the "required" column is important. If "required" for a variable is TRUE, then
#   you must include that variable in your RcensusPkg::get_vintage_data() request.
#   The variable must appear either in the "vars" vector parameter or in a "predicates" phrase.
#   If not included, an error from the API occurs.

# We are interested in values for just the state of Kentucky, so let's check that
#   the geography "state" is available for the "acs/acs1/profile" dataset.

acs1_geo <- RcensusPkg::get_geography(
  dataset = "acs/acs1/profile",
  vintage = 2021
)

# We see that "state" is one of many geographies available for 2021.

# So we are finally ready to submit a request.
# We will need the state FIPS code for Kentucky
kentucky_fips <- usmap::fips("KY")
# Submit the request.
ky_computers_broadband_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2021,
  vars = c("DP02_0152E", "DP02_0153PE","DP02_0154PE"),
  region = paste0("state:", kentucky_fips)
)

# We'll set the column names to be more intuitive
data.table::setnames(
  ky_computers_broadband_dt,
  old = c("DP02_0152E", "DP02_0153PE","DP02_0154PE"),
  new = c("Total", "ComputerUse", "BroadbandUse")
)

# You may notice that the data from the Bureau will be in a "wide" format where each variable's value
#   will be in a separate column.
# We will use  RcensusPkg's helper function wide_to_long() to put the returned data.table into
#   a "long" format where we consolidate our computer related values under one column.
#
ky_computers_broadband_long_dt <- RcensusPkg::wide_to_long(
  dt = ky_computers_broadband_dt,
  measure_v = c("Total", "ComputerUse", "BroadbandUse"),
  variable_name = "Computer Variable",
  value_name = "Value"
)
ky_computers_broadband_dt <- data.table::melt(ky_computers_broadband_dt, measure.vars = c("Total", "ComputerUse", "BroadbandUse"))

# The result is 90.2% for computers and 87.1% for broadband over 1,748,682 total households.
# These numbers can be checked by going to https://data.census.gov/all and using the Bureau's
#   search tool.

