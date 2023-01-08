library(jsonlite)
library(data.table)
library(httr)
library(RcensusPkg)

# Get descriptions/vintages for all available datasets for all years under Decennial Census data (2000, 2010, 2020)
dec_datasets_2_dt <- RcensusPkg::get_dataset_names(
  filter_name_str = "dec/"
)

# Get descriptions/vintages for all available datasets for all years under American Community Survey 1-year data (2005-2021)
acs1_datasets_dt <- RcensusPkg::get_dataset_names(
  filter_title_str = "1-year"
)

# Get descriptions on all available datasets under the Population Estimates Program (PEP) for the year 2019
pep_datasets_dt <- RcensusPkg::get_dataset_names(
  filter_title_str = "population estimates",
  vintage = 2019
)

