library(data.table)
library(magrittr)
library(httr)
library(here)
library(ggplot2)
library(RspatialPkg)
library(RplotterPkg)
library(RcensusPkg)

# -----------2000 Decennial data--------------------
# Description of Summary Files 1,2,3,4: https://www.census.gov/data/developers/data-sets/decennial-census.2000.html#list-tab-533552149

# Get metadata of the variable group "HOUSEHOLD SIZE [8]" ("H013") from the Summary File 1 ("dec/sf1") dataset.
household_size_vars_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf1",
  vintage = 2000,
  group = "H013"
) %>%
 .[, .(name, label = stringr::str_remove_all(label, "Total!!"), concept, predicateType)]

# Get count estimates for household size across the US for 2000.
household_size_2000_dt <- get_vintage_data(
  dataset = "dec/sf1",
  vintage = 2000,
  vars = household_size_vars_dt$name,
  region = "us:1"
)[,1:9] %>%
 data.table::melt(id.vars = "NAME", measure.vars = household_size_vars_dt$name) %>%
 .[, .(value = as.numeric(value))] %>%
 .[, `:=`(percent = round(value/value[1] * 100, digits = 1), label = household_size_vars_dt$label)]


# Create a bar plot of the household sizes
household_size_plot <- RplotterPkg::create_bar_plot(
  df = household_size_2000_dt[2:8,],
  aes_x = "label",
  aes_y = "percent",
  do_coord_flip = T,
  rot_y_tic_label = T,
  bar_fill = "green",
  order_bars = "desc",
  bar_labels = T
)
household_size_plot

# Using Summary File 2 ("dec/sf2") get the variable metadata that gives the total
#  households with a foster child ("PCT023006") and total households
#  with non-relatives ("PCT023001").
foster_child_in_household_total_label_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf2",
  vintage = 2000,
  vars = c("PCT023006", "PCT023001")
) %>%
 .[, .(name, label = stringr::str_remove_all(label, "Total!!"))]

# Get the total number of households with a foster child, the total households
# with non-relatives, and the percentage of non-relative households that have a
# a foster child by state for 2000.
foster_total_2000_dt <- RcensusPkg::get_vintage_data(
  dataset = "dec/sf2",
  vintage = 2000,
  vars = c("PCT023006", "PCT023001"),
  region = "state:*"
) %>%
  data.table::setnames(old = c("PCT023006","PCT023001"), new = c("households_with_foster", "households_with_nonrelatives")) %>%
  .[, .(NAME, GEOID, households_with_foster = as.numeric(households_with_foster), households_with_nonrelatives = as.numeric(households_with_nonrelatives))] %>%
  .[, percent_with_foster := round(households_with_foster/households_with_nonrelatives * 100, digits = 1)]

# Plot the US states showing their respective percentage of households with a foster child among
#   households with non-relatives.
output_dir <- file.path(here(), "", "shapefiles")
foster_percent_sf <- RcensusPkg::tiger_states_sf(
  output_dir = output_dir,
  vintage = 2000,
  datafile = foster_total_2000_dt,
  datafile_key = "NAME",
  sf_key = "NAME",
  general = T,
  sf_info = F
)
RspatialPkg::get_geom_sf(
  sf = foster_percent_sf,
  aes_fill = "percent_with_foster",
  hide_x_tics = T,
  hide_y_tics = T
) +
  ggplot2::coord_sf(
    xlim = c(-179.0, -60.0),
    ylim = c(15.0, 72.0)
  )

# Using Summary File 3 ("dec/sf3") get the variable metadata that gives the total
#  urban and rural counts ("H005002" and "H005005" respectively) and total urban + rural total
#  ("H005001").
urban_rural_names_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf3",
  vintage = 2000,
  vars = c("H005002", "H005005", "H005001")
) %>%
 .[, .(name, label = stringr::str_remove_all(label, "Total!!"))]

# Get the counts for urban and rural along with the total urban + rural for 2000
#  across the states.
urban_rural_2000_dt <- RcensusPkg::get_vintage_data(
  dataset = "dec/sf3",
  vintage = 2000,
  vars = c("H005002", "H005005", "H005001"),
  region = "state:*"
) %>%
  data.table::setnames(old = c("H005002", "H005005", "H005001"), new = c("urban","rural","urban_rural")) %>%
  .[, .(NAME, GEOID, urban = as.numeric(urban), rural = as.numeric(rural), urban_rural = as.numeric(urban_rural))] %>%
  .[, percent_rural := round(rural/urban_rural * 100, digits = 1)]

# Plot the US states showing their respective percentage of rural households
output_dir <- file.path(here(), "", "shapefiles")
rural_percent_sf <- RcensusPkg::tiger_states_sf(
  output_dir = output_dir,
  vintage = 2000,
  datafile = urban_rural_2000_dt,
  datafile_key = "NAME",
  sf_key = "NAME",
  general = T,
  sf_info = F
)
RspatialPkg::get_geom_sf(
  sf = rural_percent_sf,
  aes_fill = "percent_rural",
  hide_x_tics = T,
  hide_y_tics = T
) +
  ggplot2::coord_sf(
    xlim = c(-179.0, -60.0),
    ylim = c(15.0, 72.0)
  )

# Using Summary File 4 ("dec/sf4") get the variable metadata that gives the counts
#  of individuals whose income for 1999 was below the poverty level ("PCT148002"),
#  and individuals below the poverty level with/without a disability ("PCT148003" and
#  "PCT148013" respectively) for 2000.

poverty_disability_2000_names_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/sf4",
  vintage = 2000,
  vars = c("PCT148003", "PCT148013", "PCT148002")
)%>%
 .[, .(name, label = stringr::str_remove_all(label, "Total!!"))]

# Get the counts of individuals below the poverty level with/without disabilities by state for 2000.
poverty_disability_2000_dt <- RcensusPkg::get_vintage_data(
  dataset = "dec/sf4",
  vintage = 2000,
  vars = c("PCT148003", "PCT148013", "PCT148002"),
  region = "state:*"
)%>%
  data.table::setnames(old = c("PCT148003", "PCT148013", "PCT148002"), new = c("poverty_with_disability","poverty_without_disability","total_poverty")) %>%
  .[, .(NAME, GEOID, poverty_with_disability = as.numeric(poverty_with_disability), poverty_without_disability = as.numeric(poverty_without_disability), total_poverty = as.numeric(total_poverty))] %>%
  .[, percent_poverty_with_disability := round(poverty_with_disability/total_poverty * 100, digits = 1)]

# Plot the US states showing their respective percentage of individuals below the poverty
#  level with a disability.
output_dir <- file.path(here(), "", "shapefiles")
rural_percent_sf <- RcensusPkg::tiger_states_sf(
  output_dir = output_dir,
  vintage = 2000,
  datafile = poverty_disability_2000_dt,
  datafile_key = "NAME",
  sf_key = "NAME",
  general = T,
  sf_info = F
)
RspatialPkg::get_geom_sf(
  sf = rural_percent_sf,
  aes_fill = "percent_poverty_with_disability",
  hide_x_tics = T,
  hide_y_tics = T
) +
  ggplot2::coord_sf(
    xlim = c(-179.0, -60.0),
    ylim = c(15.0, 72.0)
  )

