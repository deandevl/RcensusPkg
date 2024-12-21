library(data.table)
library(httr)
library(stringr)
library(ggplot2)
library(RplotterPkg)
library(RcensusPkg)

# -------American Community Survey (ACS) 1-year Data (2005-2021)------
# Description: https://www.census.gov/data/developers/data-sets/acs-1year.html
#
# Get variable metadata for the group "SEX BY AGE" ("B01001") from the ACS
#  1-year dataset for 2021
sex_by_age_names_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs1",
  vintage = 2021,
  group = "B01001",
  filter_group_est = TRUE
) |>
 _[, .(name, label = stringr::str_remove_all(label, "Estimate!!Total:!!"), predicateType)]

sex_by_age_names_dt$label[[1]] <- "Total"

# Get count estimates for all 49 of the group variables in US for 2021
#  and add a percentage column.
sex_by_age_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1",
  vintage = 2021,
  group = "B01001",
  region = "us:1",
  wide_to_long = TRUE
) |>
 _[, .(variable, estimate = as.numeric(estimate))] |>
 _[, `:=`(label = sex_by_age_names_dt$label, percent = round(estimate/estimate[1] * 100, digits = 1))]

# Divide the datatable sex_by_age_dt into male and female datatables, modify their "Label" columns, and add a "sex" column
male_dt <- sex_by_age_dt[grepl("Male", sex_by_age_dt$label, fixed = T),][2:24] |>
 _[, `:=` (label = stringr::str_remove(label, "Male:!!"),sex = "Male")]

female_dt <- sex_by_age_dt[grepl("Female", sex_by_age_dt$label, fixed = T),][2:24] |>
  _[, `:=` (label = stringr::str_remove(label, "Female:!!"),sex = "Female")]

# Row bind male_dt and female_dt datatables and set "sex" and "label" as factors
male_female_dt <- rbind(male_dt, female_dt)
male_female_dt[, `:=`(sex = as.factor(sex), label = factor(label, levels = male_female_dt$label[1:23]))]

# Create a bar plot of the male/female datatable:
RplotterPkg::create_bar_plot(
  df = male_female_dt,
  aes_x = "label",
  aes_y = "percent",
  aes_fill = "sex",
  do_coord_flip = TRUE,
  position = "dodge",
  rot_y_tic_label = TRUE,
  x_title = "Percent"
)
