library(jsonlite)
library(data.table)
library(httr)
library(usmap)
library(RcensusPkg)

# The following R script demonstrates using the
# RcensusPkg::wide_to_long() function.

# Goal: Get the migration flow data for Honolulu, HI. Reshape the
#  resultant "wide" shape of 6 column variables to two column
#  variables named "estimate" (the data values) and "moe"
#  (the margin of error values).

# County is Honolulu
honolulu_fips <- usmap::fips(state = "HI", county = "Honolulu")

# Consolidate 6 variable columns into 2 variable columns of "estimate" and "moe"
honolulu_migration_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/flows",
  vintage = 2019,
  NAME_GEOID = FALSE,
  vars = c("FULL1_NAME", "FULL2_NAME", "MOVEDIN", "MOVEDIN_M","MOVEDOUT", "MOVEDOUT_M","MOVEDNET","MOVEDNET_M"),
  region = paste0("county:", substr(honolulu_fips,3,5)),
  regionin = paste0("state:", substr(honolulu_fips,1,2))
)

migration_est_dt <- RcensusPkg::wide_to_long(
  dt = honolulu_migration_dt,
  id_v = c("FULL1_NAME", "FULL2_NAME"),
  measure_v = c("MOVEDIN", "MOVEDOUT", "MOVEDNET"),
  variable_name = "variable",
  value_name = "estimate"
)

migration_moe_dt <- RcensusPkg::wide_to_long(
  dt = honolulu_migration_dt,
  id_v = c("FULL1_NAME", "FULL2_NAME"),
  measure_v = c("MOVEDIN_M", "MOVEDOUT_M", "MOVEDNET_M"),
  variable_name = "variable",
  value_name = "moe"
)

honolulu_migration_long_dt <- migration_est_dt[, moe := migration_moe_dt$moe]
