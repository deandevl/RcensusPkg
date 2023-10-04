library(data.table)
library(magrittr)
library(httr)
library(ggplot2)
library(usmap)
library(RplotterPkg)
library(RcensusPkg)

# -----------2020 Decennial data--------------------
# Description of Demographic and Housing Characteristics File (DHC): https://www.census.gov/data/developers/data-sets/decennial-census.2020.html#list-tab-533552149

# From the 2020 DHC get the counts for household size from 1 to 7 people (group variables "H12I") in Holmes and Geauga counties, Ohio.
# Note: Holmes County has a large Amish population.  Geauga County is near Cleveland, Ohio.

holmes_ohio_fips <- usmap::fips(state = "Ohio", county = "Holmes")
geauga_ohio_fips <- usmap::fips(state = "Ohio", county = "Geauga")
ohio_fips <- substr(holmes_ohio_fips, 1, 2)
holmes_fips <- substr(holmes_ohio_fips, 3, 5)
geauga_fips <- substr(geauga_ohio_fips, 3, 5)

# Get metadata of the variable group "TENURE BY HOUSEHOLD SIZE (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)" ("H12I")
household_size_vars_dt <- RcensusPkg::get_variable_names(
  dataset = "dec/dhc",
  vintage = 2020,
  group = "H12I"
) %>%
  .[predicateType == "int",] %>%
  .[2:9] %>%
  .[, .(name, label = stringr::str_remove_all(label, "!!Total:!!Owner occupied:!!"))]
household_size_vars_dt[[1,2]] <- "Total"

get_household_size_count <- function(county_fips){
  county_household_size_count_dt <- RcensusPkg::get_vintage_data(
  dataset = "dec/dhc",
  vintage = 2020,
  group = "H12I",
  region = paste0("county:", county_fips),
  regionin = paste0("state:", ohio_fips),
) %>%
  data.table::melt(id.vars = "NAME") %>%
  na.omit() %>%
  .[2:9, .(NAME, variable, value = as.numeric(value))] %>%
  .[, `:=`(label =  household_size_vars_dt$label, percent = round(value/value[[1]],digits = 2))] %>%
  .[2:8]

  return(county_household_size_count_dt)
}

holmes_ohio_household_size_count_dt <- get_household_size_count(county_fips = holmes_fips)
geauga_ohio_household_size_count_dt <- get_household_size_count(county_fips = geauga_fips)

holmes_geauga_household_size_count_dt <- rbind(holmes_ohio_household_size_count_dt, geauga_ohio_household_size_count_dt)

holmes_geauga_household_size_plot <- RplotterPkg::create_bar_plot(
  df = holmes_geauga_household_size_count_dt,
  aes_x = "label",
  aes_y = "percent",
  aes_fill = "NAME",
  position = "dodge",
  x_title = "percent",
  rot_y_tic_label = T,
  do_coord_flip = T
)
holmes_geauga_household_size_plot