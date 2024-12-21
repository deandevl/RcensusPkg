library(data.table)
library(httr)
library(here)
library(ggplot2)
library(RspatialPkg)
library(RplotterPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

# -----------2000 Decennial data--------------------
# Description of Summary Files 1,2,3,4: https://www.census.gov/data/developers/data-sets/decennial-census.2000.html#list-tab-533552149

# Get metadata of the variable group "HOUSEHOLD SIZE [8]" ("H013") from the Summary File 1 ("dec/sf1") dataset.
household_size_vars_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf1",
  vintage = 2000,
  group = "H013"
) |>
 _[, .(name, label = stringr::str_remove_all(label, "Total!!"), concept, predicateType)]

# Get count estimates for household size across the US for 2000.
labels <- household_size_vars_dt$label[3:9]
household_size_2000_dt <- get_vintage_data(
  dataset = "dec/sf1",
  vintage = 2000,
  vars = household_size_vars_dt$name,
  region = "us:1"
)[,1:10] |>
 data.table::melt(id.vars = c("NAME", "GEO_ID")) |>
 _[, .(variable, value = as.numeric(value))] |>
 _[, `:=`(percent = round(value/value[1] * 100, digits = 1))] |>
 _[2:8, ] |>
 _[, label := factor(labels, levels = labels)]

# Create a bar plot of the household sizes
RplotterPkg::create_bar_plot(
  df = household_size_2000_dt,
  aes_x = "label",
  aes_y = "percent",
  do_coord_flip = TRUE,
  rot_y_tic_label = TRUE,
  bar_fill = "green",
  bar_labels = TRUE,
  x_title = "Percent"
)

# Using Summary File 2 ("dec/sf2") get the variable metadata that gives the total
#  households with a foster child ("PCT023006") and total households
#  with non-relatives ("PCT023001").
foster_child_in_household_total_label_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf2",
  vintage = 2000,
  vars = c("PCT023006", "PCT023001")
) |>
 _[, .(name, label = stringr::str_remove_all(label, "Total!!"))]

# Get the total number of households with a foster child, the total households
# with non-relatives, and the percentage of non-relative households that have a
# a foster child by state for 2000.
foster_total_2000_dt <- RcensusPkg::get_vintage_data(
  dataset = "dec/sf2",
  vintage = 2000,
  vars = c("PCT023006", "PCT023001"),
  region = "state:*"
) |>
  data.table::setnames(old = c("PCT023006","PCT023001"), new = c("households_with_foster", "households_with_nonrelatives")) |>
  _[, .(NAME, GEOID, households_with_foster = as.numeric(households_with_foster), households_with_nonrelatives = as.numeric(households_with_nonrelatives))] |>
  _[, percent_with_foster := round(households_with_foster/households_with_nonrelatives * 100, digits = 1)]

# Plot the US states showing their respective percentage of households with a foster child among
#   households with non-relatives.
RcensusPkg::plot_us_data(
  df = foster_total_2000_dt,
  states_col = "NAME",
  value_col = "percent_with_foster",
  output_dir = output_dir,
  scale_breaks = seq(0,6,1),
  scale_limits = c(0,6),
  scale_labels = seq(0,6,1)
)

# Using Summary File 3 ("dec/sf3") get the variable metadata that gives the total
#  urban and rural counts ("H005002" and "H005005" respectively) and total urban + rural total
#  ("H005001").
urban_rural_names_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf3",
  vintage = 2000,
  vars = c("H005002", "H005005", "H005001")
) |>
 _[, .(name, label = stringr::str_remove_all(label, "Total!!"))]

# Get the counts for urban and rural along with the total urban + rural for 2000
#  across the states.
urban_rural_2000_dt <- RcensusPkg::get_vintage_data(
  dataset = "dec/sf3",
  vintage = 2000,
  vars = c("H005002", "H005005", "H005001"),
  region = "state:*"
) |>
  data.table::setnames(old = c("H005002", "H005005", "H005001"), new = c("urban","rural","urban_rural")) |>
  _[, .(NAME, GEOID, urban = as.numeric(urban), rural = as.numeric(rural), urban_rural = as.numeric(urban_rural))] |>
  _[, percent_rural := round(rural/urban_rural * 100, digits = 1)]

# Plot the US states showing their respective percentage of rural households
RcensusPkg::plot_us_data(
  df = urban_rural_2000_dt,
  states_col = "NAME",
  value_col = "percent_rural",
  output_dir = output_dir,
  scale_breaks = seq(0,70,10),
  scale_limits = c(0,70),
  scale_labels = seq(0,70,10)
)

# Using Summary File 4 ("dec/sf4") get the variable metadata that gives the counts
#  of individuals whose income for 1999 was below the poverty level ("PCT148002"),
#  and individuals below the poverty level with/without a disability ("PCT148003" and
#  "PCT148013" respectively) for 2000.

poverty_disability_2000_names_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf4",
  vintage = 2000,
  vars = c("PCT148003", "PCT148013", "PCT148002")
) |>
 _[, .(name, label = stringr::str_remove_all(label, "Total!!"))]

# Get the counts of individuals below the poverty level with/without disabilities by state for 2000.
poverty_disability_2000_dt <- RcensusPkg::get_vintage_data(
  dataset = "dec/sf4",
  vintage = 2000,
  vars = c("PCT148003", "PCT148013", "PCT148002"),
  region = "state:*"
) |>
  data.table::setnames(old = c("PCT148003", "PCT148013", "PCT148002"), new = c("poverty_with_disability","poverty_without_disability","total_poverty")) |>
  _[, .(NAME, GEOID, poverty_with_disability = as.numeric(poverty_with_disability), poverty_without_disability = as.numeric(poverty_without_disability), total_poverty = as.numeric(total_poverty))] |>
  _[, percent_poverty_with_disability := round(poverty_with_disability/total_poverty * 100, digits = 1)]

# Plot the US states showing their respective percentage of individuals below the poverty
#  level with a disability.
RcensusPkg::plot_us_data(
  df = poverty_disability_2000_dt,
  states_col = "NAME",
  value_col = "percent_poverty_with_disability",
  output_dir = output_dir,
  scale_breaks = seq(20,40,5),
  scale_limits = c(20,40),
  scale_labels = seq(20,40,5)
)
