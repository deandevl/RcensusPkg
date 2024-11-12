library(jsonlite)
library(data.table)
library(httr)
library(usmap)
library(RcensusPkg)

# The following R script demonstrates using the
# data.table::melt()

# Goal: Get the migration flow data for Honolulu, HI. Reshape the
#  resultant "wide" shape of 2 id variables and 6 measured variables to four columns.
#  Similarly, reshape the resultant "wide" shape of 2 id variables and 6 moe variables to four columns.
#  Finally combine both the long versions of estimate and moe to one data.table.

# County is Honolulu
honolulu_fips <- usmap::fips(state = "HI", county = "Honolulu")

# Get the 3 estimate and 3 moe variables along with 2 "name" variables
honolulu_migration_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/flows",
  vintage = 2019,
  NAME_GEOID = FALSE,
  vars = c("FULL1_NAME", "FULL2_NAME", "MOVEDIN", "MOVEDIN_M","MOVEDOUT", "MOVEDOUT_M","MOVEDNET","MOVEDNET_M"),
  region = paste0("county:", substr(honolulu_fips,3,5)),
  regionin = paste0("state:", substr(honolulu_fips,1,2))
)

# Consolidate the 3 estimate variable columns into 2 variable columns of "variable" and "estimate"
#   using data.table::melt()
honolulu_migration_long_est_dt <- data.table::melt(
  honolulu_migration_dt,
  id.vars = c("FULL1_NAME", "FULL2_NAME"),
  measure.vars = c("MOVEDIN", "MOVEDOUT", "MOVEDNET"),
  variable.name = "variable",
  value.name = "estimate"
)
# Similarly consolidate the 3 moe variables
honolulu_migration_long_moe_dt <- data.table::melt(
  honolulu_migration_dt,
  id.vars = c("FULL1_NAME", "FULL2_NAME"),
  measure.vars = c("MOVEDIN_M", "MOVEDOUT_M", "MOVEDNET_M"),
  variable.name = "variable",
  value.name = "moe"
)
# Combine the estimate and moe datatables.
honolulu_migration_long_dt <- honolulu_migration_long_est_dt[, moe := honolulu_migration_long_moe_dt$moe]
