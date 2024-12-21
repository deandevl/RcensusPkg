library(data.table)
library(httr)
library(RcensusPkg)

# 1year wide form, default variables, 2 countries demo
one_year_dt <- RcensusPkg::get_idb_data(
  dataset = "1year",
  years = c(2023, 2024),
  countries = c("BW", "NO")
)

# 1year wide form, selected variable (sex = female) demo
one_year_female_dt <- RcensusPkg::get_idb_data(
  dataset = "1year",
  years = c(2023, 2024),
  countries = c("BW", "NO"),
  sex = 2
)

# 1year wide form, selected variable (sex = female),
# ages 65 to 70, all countries demo
one_year_female_65_70_countries_dt <- RcensusPkg::get_idb_data(
  dataset = "1year",
  years = 2023,
  sex = 2,
  ages = 65:70
)

# 5year wide form, default variables demo
five_year_dt <- RcensusPkg::get_idb_data(
  dataset = "5year",
  years = c(2023, 2024),
  countries = c("BW", "NO")
)

# 5year wide form, default variables, 2023, all countries demo
five_year_countries_2023_dt <- RcensusPkg::get_idb_data(
  dataset = "5year",
  years = 2023
)

# 5year wide form, all variables, 2023, country = US demo
five_year_US_group_dt <- RcensusPkg::get_idb_data(
  dataset = "5year",
  years = 2023,
  group = TRUE,
  countries = "US"
)

# 5year long form, all variables, 2023, country = US demo
five_year_US_long_group_dt <- RcensusPkg::get_idb_data(
  dataset = "5year",
  years = 2023,
  group = TRUE,
  countries = "US",
  wide_to_long = TRUE
)
