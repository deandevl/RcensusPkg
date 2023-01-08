library(jsonlite)
library(data.table)
library(magrittr)
library(httr)
library(usmap)
library(RcensusPkg)

# Get multiple years (2010 to 2019) of numbers of college degree holders
#   across all counties in Colorado

# Get the fips for Colorado
co_fips <- usmap::fips("CO")

# Set up the the vintage years and variables
vintages <- 2010:2019

college_var_names <- c(
  "B15002_015",
  "B15002_016",
  "B15002_017",
  "B15002_018",
  "B15002_032",
  "B15002_033",
  "B15002_034",
  "B15002_035"
)

vars <- c()
for(i in seq_along(college_var_names)){
  vars <- c(vars, paste0(college_var_names[[i]],"E"))
  vars <- c(vars, paste0(college_var_names[[i]],"M"))
}

# Get the data.table. Returns data.table in "wide" format.
college_by_year_dt <- RcensusPkg::get_multi_vintage_data(
  dataset = "acs/acs1",
  vintage_v = vintages,
  vars = vars,
  region = "county:*",
  regionin = paste0("state:", co_fips)
) %>%
  .[,`:=`(state = NULL, county = NULL)]

# Reshape college_by_year_dt in "long" format.
college_by_year_long_dt <- RcensusPkg::wide_to_long(
  dt = college_by_year_dt,
  id_v = c("NAME","GEOID","vintage"),
  do_est_moe = TRUE
)



