library(data.table)
library(magrittr)
library(httr)
library(RcensusPkg)

# ------American Community Survey 5-Year Data (2009-2021)-----------
# Description: https://www.census.gov/data/developers/data-sets/acs-5year.html

# Get variable metadata on white females ("B01001A_017E") and total white
#   individuals ("B01001A_001E") from the ACS 5-Year dataset for 2021.
acs_5_vars_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs5",
  vars = c("B01001A_017E", "B01001A_001E"),
  vintage = 2021
)
# Get the data for the total number of white females and total number of white
#   individuals and compute the percentage of white females by state.
white_females_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs5",
  vars = c("B01001A_017E", "B01001A_001E"),
  vintage = 2021,
  region = "state:*"
) %>%
  data.table::setnames(old = c("B01001A_017E", "B01001A_001E"), new = c("white_females", "total_white")) %>%
  .[, .(NAME, white_females = as.numeric(white_females), total_white = as.numeric(total_white))]  %>%
  .[, percent_female := round(white_females/total_white * 100, digits = 1)]

