library(jsonlite)
library(data.table)
library(httr)
library(RcensusPkg)

# Get data.table of variables for 'acs/acs5' for 2019
acs5_var_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs1/profile",
  vintage =  2019)

# Get available variables for group "DP02" in the
#  "acs/acs1/profile" dataset for 2019
group_var_DP02_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  group = "DP02"
)

# Get available variables that have the phrase "educational attainment" in "label"
#  column of the resultant data.table.
educational_attainment_2019_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  filter_label_str = "educational attainment"
)

# Get available variables that have the filtering string "DP02_0068" in their acronym name.
DP02_0068_vars_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs1/profile",
  vintage = 2019,
  filter_name_str = "dp02_0068"
)

# Get descriptions of a set of variables
vars <- c("B15002_015E", "B15002_015M",
          "B15002_016E", "B15002_016M",
          "B15002_017E", "B15002_017M",
          "B15002_018E", "B15002_018M",
          "B15002_032E", "B15002_032M",
          "B15002_033E", "B15002_033M",
          "B15002_034E", "B15002_034M",
          "B15002_035E", "B15002_035M"
        )
vars_B15002_dt <- RcensusPkg::get_variable_names(
  dataset = "acs/acs1",
  vintage = 2019,
  vars = vars
)
