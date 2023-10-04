library(data.table)
library(magrittr)
library(httr)
library(ggplot2)
library(RplotterPkg)
library(RcensusPkg)

# -----American Community Survey 1-year Supplemental Data (2014-2021)------
# Description: https://www.census.gov/data/developers/data-sets/ACS-supplemental-data.html

# Get variable metadata of the group "MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER"
#  ("K201001") from the ACS 1-year Supplemental dataset for 2021
marital_status_names_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acsse",
  group = "K201001",
  filter_group_est = T,
  vintage = 2021
) %>%
 .[, .(name, label = stringr::str_remove_all(label, "Estimate!!Total:!!"), predicateType)]

marital_status_names_dt$label[[1]] <- "Total"

# Get count estimates for all 6 of the group variables in US for 2021
#  and add a percentage column.
marital_status_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acsse",
  vintage = 2021,
  group = "K201001",
  region = "US:*"
) %>%
  data.table::melt(id.v = "NAME", measure.vars = marital_status_names_dt$name) %>%
 .[, .(value = as.numeric(value))] %>%
 .[, `:=`(Label = marital_status_names_dt$label, Percent = round(value/value[1] * 100, digits = 1))] %>%
 .[Label != "Total"] %>%
 .[, Label := factor(Label, levels = marital_status_names_dt$label[2:6])]

# Create a bar plot of the US marital status
marital_status_plot <- RplotterPkg::create_bar_plot(
  df = marital_status_dt,
  aes_x = "Label",
  aes_y = "Percent",
  do_coord_flip = T,
  position = "dodge",
  rot_y_tic_label = T,
  order_bars = "desc",
  bar_labels = T,
  bar_fill = "green",
  bar_alpha = 0.7
)
marital_status_plot
