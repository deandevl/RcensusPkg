library(httr)
library(data.table)
library(RcensusPkg)

# The following R script demonstrates using the
# RcensusPkg::long_to_wide() function.

# Goal: Get Census Bureau data for the "B19001" group of variables.
# The resultant data.table will have the variables in the "long"
# format. Reshape the data.table so that the variables (columns)
# appear in the "wide" format.

# Get a group of Census Bureau parameters
# The returning data.table will be in the "long" format
B19001_long_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1",
  vintage = 2016,
  group = "B19001",
  region = "state"
) %>%
.[order(NAME)]

# Now reshape the data.table to the "wide" format for "estimate" column values
B19001_est_wide_dt <- RcensusPkg::long_to_wide(
  dt = B19001_long_dt,
  parameter_col = "variable",
  value_col = "estimate"
)

# Reshape the data.table to the "wide" format for both "estimate" and "moe"
#   column values.
B19001_est_moe_wide_dt <- RcensusPkg::long_to_wide(
  dt = B19001_long_dt,
  parameter_col = "variable",
  value_col = c("estimate", "moe")
)



