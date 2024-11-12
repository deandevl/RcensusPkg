library(data.table)
library(magrittr)
library(httr)
library(ggplot2)
library(RcensusPkg)

# ------------------CPS ASEC Poverty Statistics (1959-2021) data --------------------
# Description of CPS ASEC Poverty Statistics ("timeseries/poverty/histpov2"):
#   https://www.census.gov/data/developers/data-sets/Poverty-Statistics.html

# Get the available geographies for "timeseries/poverty/histpov2"
histpov2_geos_dt <- RcensusPkg::get_geography(
  dataset = "timeseries/poverty/histpov2"
)
# Note: Only one geography, i.e. "us"

# Get the percent poverty for white, black, asian, hispanic over years 2002 to 2021
percent_poverty_dt <- RcensusPkg::get_vintage_data(
  dataset = "timeseries/poverty/histpov2",
  vars = "PCTPOV",
  predicates = "&time=from+2002+to+2021&RACE=2&RACE=7",
  region = "us:*"
) %>%
  .[, .(time = as.integer(time), PCTPOV = as.numeric(PCTPOV), RACE = as.integer(RACE))] %>%
  data.table::setnames(old = "PCTPOV", new = "percent_poverty")

# Do some data wrangling
percent_poverty_dt$RACE <- data.table::fifelse(percent_poverty_dt$RACE == 2, "white", "black")
percent_poverty_dt$RACE <- as.factor(percent_poverty_dt$RACE)

percent_poverty_dt$percent_poverty <- data.table::fifelse(
  percent_poverty_dt$time == 2017 & percent_poverty_dt$RACE == "white",
  10.6,
  percent_poverty_dt$percent_poverty
)
percent_poverty_dt$percent_poverty <- data.table::fifelse(
  percent_poverty_dt$time == 2013 & percent_poverty_dt$RACE == "white",
  12.6,
  percent_poverty_dt$percent_poverty
)

percent_poverty_dt$percent_poverty <- data.table::fifelse(
  percent_poverty_dt$time == 2017 & percent_poverty_dt$RACE == "black",
  21.45,
  percent_poverty_dt$percent_poverty
)
percent_poverty_dt$percent_poverty <- data.table::fifelse(
  percent_poverty_dt$time == 2013 & percent_poverty_dt$RACE == "black",
  26.2,
  percent_poverty_dt$percent_poverty
)

poverty_plot <- RplotterPkg::create_scatter_plot(
  df = percent_poverty_dt,
  aes_x = "time",
  aes_y = "percent_poverty",
  aes_color = "RACE",
  connect = T,
  x_major_breaks = seq(from = 2002, to = 2022, by = 2),
  y_major_breaks = seq(from = 0, to = 30, by = 2)
)
poverty_plot
