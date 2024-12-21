library(data.table)
library(httr)
library(stringr)
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
  filter_group_est = TRUE,
  vintage = 2021
) |>
_[, .(name, label = stringr::str_remove_all(label, "Estimate!!Total:!!"), predicateType)] |>
_[1:6,]

marital_status_names_dt$label[[1]] <- "Total"

# Get count estimates for all 6 of the group variables in US for 2021
#  and add a percentage column.
marital_status_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acsse",
  vintage = 2021,
  group = "K201001",
  region = "US:*",
  wide_to_long = TRUE
) |>
 _[, .(estimate = as.numeric(estimate))] |>
 _[, `:=`(Label = marital_status_names_dt$label, Percent = round(estimate/estimate[1] * 100, digits = 1))] |>
 _[Label != "Total"] |>
 _[, Label := factor(Label, levels = marital_status_names_dt$label[2:6])] |>
  na.omit()

# Create a bar plot of the US marital status
RplotterPkg::create_bar_plot(
  df = marital_status_dt,
  aes_x = "Label",
  aes_y = "Percent",
  do_coord_flip = TRUE,
  position = "dodge",
  rot_y_tic_label = TRUE,
  order_bars = "desc",
  bar_labels = TRUE,
  bar_fill = "green",
  bar_alpha = 0.7,
  x_title = "Percent"
)

# Find the same statistics across the states
sel_columns <- c("NAME", marital_status_names_dt$name)
name_columns <- c("State", "Total", "Never_Married", "Now_Married", "Separated", "Widowed", "Divorced")
marital_status_states_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acsse",
  vintage = 2023,
  group = "K201001",
  region = "state:*"
) |>
_[, ..sel_columns] |>
data.table::setnames(old = sel_columns, new = name_columns) |>
_[, `:=`(
  Total = as.numeric(Total),
  Never_Married = as.numeric(Never_Married),
  Now_Married = as.numeric(Now_Married),
  Separated = as.numeric(Separated),
  Widowed = as.numeric(Widowed),
  Divorced = as.numeric(Divorced)
 )] |>
_[, Alone_pct := round((Never_Married + Separated + Widowed + Divorced)/Total * 100, digits = 1)] |>
_[, `:=`(
  Never_Married_pct = round(Never_Married/Total * 100, digits = 1),
  Now_Married_pct = round(Now_Married/Total * 100, digits = 1),
  Separated_pct = round(Separated/Total * 100, digits = 1),
  Widowed_pct = round(Widowed/Total * 100, digits = 1),
  Divorced_pct = round(Divorced/Total * 100, digits = 1)
)]
