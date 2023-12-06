library(data.table)
library(magrittr)
library(httr)
library(ggplot2)
library(usmap)
library(here)
library(RspatialPkg)
library(RcensusPkg)

# Support data
output_dir <- file.path(here(), "demo", "shapefiles")
ohio_fips <- usmap::fips(state = "Ohio")

# ---------------2002, 2007, 2012, 2017 Economic Census data --------------
# Description of Economic Census ("ecnbasic"): https://www.census.gov/data/developers/data-sets/economic-census.html
# Description for 2017:
# "This dataset provides industry statistics on number of firms; number of establishments; number of employees;
#    payroll; and sales, value of shipments, or revenue by geographic area for establishments and firms with paid
#    employees. Data are shown on a 2017 NAICS basis. The statistics, NAICS levels, and the geographic areas
#    covered vary by sector."

# The NAICS2017 codes for the most frequent sectors with statistics are:
#     Construction - "23"
#     Manufacturing - "31-33"
#     Mining - "21"

# Get the labels and codes for the North American Industry Classification System (https://www.census.gov/naics)
naics_codes_dt <- RcensusPkg::get_vintage_data(
  dataset = "ecnbasic",
  vintage = 2017,
  vars = c("NAICS2017_LABEL", "NAICS2017"),
  NAME_GEOID = F,
  region = "us:*"
)

# -------------------------------------------
# Get and compare the following computer related expense variables:
#   "Data processing and other purchased computer services ($1,000)" -- "PCHDAPR"
#   "Expensed computer hardware and other equipment ($1,000)" -- "PCHCMPQ"
# for the Manufacturing ("31-33"), Construction ("23"), Mining ("21") related industries for the entire US.

us_computer_expenses_dt <- RcensusPkg::get_vintage_data(
  dataset = "ecnbasic",
  vintage = 2017,
  vars = c("NAICS2017_LABEL", "PCHDAPR", "PCHCMPQ"),
  predicates = "&NAICS2017=31-33&NAICS2017=23&NAICS2017=21",
  region = "us:*"
)

# Do some data.table wrangling to produce a bar chart.
us_computer_expenses_long_dt <- us_computer_expenses_dt %>%
  data.table::setnames(old = c("PCHDAPR", "PCHCMPQ"), new = c("computer_services", "computer_hardware")) %>%
  data.table::melt(id.vars = "NAICS2017_LABEL", measure.vars = c("computer_services", "computer_hardware")) %>%
  .[, .(NAICS2017_LABEL, variable, value = as.integer(value))]

# Create the bar chart
us_computer_expenses_plot <- RplotterPkg::create_bar_plot(
  df = us_computer_expenses_long_dt,
  aes_x = "NAICS2017_LABEL",
  aes_y = "value",
  aes_fill = "variable",
  do_coord_flip = T,
  rot_y_tic_label = T
)
us_computer_expenses_plot

# ------------------------------------------------
# Find the number of employees ("EMP") associated with NAICS code 6211 ("Offices of physicians") among counties in Ohio.
ohio_county_physician_employees_dt <- RcensusPkg::get_vintage_data(
  dataset = "ecnbasic",
  vintage = 2017,
  vars = c("NAICS2017_LABEL", "EMP"),
  predicates = "&NAICS2017=6211",
  region = "county:*",
  regionin = paste0("state:", ohio_fips)
) %>%
  .[, .(NAME = stringr::str_remove(NAME, "County, Ohio"), GEOID, county, EMP = as.numeric(EMP))]

# Get the simple feature (sf) geometries for the Ohio counties and join it with the above physician employees data
express <- expression(STATEFP == 39)
ohio_county_sf <- RcensusPkg::tiger_counties_sf(
  output_dir = output_dir,
  vintage = 2017,
  general = T,
  sf_info = F,
  datafile = ohio_county_physician_employees_dt,
  datafile_key = "GEOID",
  express = express,
  sf_key = "GEOID"
)

# There are 6 counties that did not have "EMP" values, so assign EMP = 0 to these counties
dt <- data.table::as.data.table(ohio_county_sf)
dt$EMP <- data.table::fifelse(test =  dt$EMP > 0.0, yes =  dt$EMP, no = 0.0, na = 0.0)
ohio_county_sf <- sf::st_as_sf(dt)

# Plot the Ohio counties with their degree of employees in physician offices
ohio_physicians_plot <- RspatialPkg::get_geom_sf(
  sf = ohio_county_sf,
  aes_fill = "EMP",
  aes_text = "i.NAME",
  sf_color = "white",
  text_color = "white",
  hide_x_tics = T,
  hide_y_tics = T
)
ohio_physicians_plot


# Get metadata of the variable "Capital expenditures for computers and peripheral data processing equipment($1000)"
#   ("CEXMCHC")
# capital_vars_dt <- RcensusPkg::get_vintage_data(
#   dataset = "ecnbasic",
#   vintage = 2017,
#   vars =  "CEXMCHC",
#   predicates = "&NAICS2017=2111",
#   region = "state:*",
#   regionin = "us:1"
# )

