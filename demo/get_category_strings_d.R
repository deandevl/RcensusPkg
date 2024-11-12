library(data.table)
library(magrittr)
library(usmap)
library(RcensusPkg)

# Get the names currently supported category variables
category_names_v <- RcensusPkg::get_category_strings(get_names = T)

# Get the integer/string pairs for the category name "sex"
sex_category_dt <- RcensusPkg::get_category_strings(name = "sex")

# Make a data request to get the "SEX" and "AGEGROUP" categorical variables from the "pep/charagegroups"
#  data.table for the state of Utah, 2019.

# 1. Call `Rcensus::get_vintage_data()`:
ut_fips <- usmap::fips("UT")

ut_dt <- RcensusPkg::get_vintage_data(
  dataset = "pep/charagegroups",
  vintage = 2019,
  vars = c("SEX", "AGEGROUP", "POP"),
  region = paste0("state:",ut_fips)
)

# 2. Do some minor wrangling with `ut_dt`:
ut_dt <- ut_dt %>%
  .[, `:=`(SEX = as.numeric(SEX), AGEGROUP = as.numeric(AGEGROUP), POP = as.numeric(POP))] %>% # set as numeric
  .[order(SEX, AGEGROUP)] %>%                                                                  # order by SEX, AGEGROUP
  .[SEX > 0 & AGEGROUP > 0 & AGEGROUP < 19] # categories for SEX and AGEGROUP of interest


# 3. Get the "SEX" and "AGEGROUP" integer values and string labels:
sex_categories_dt <- RcensusPkg::get_category_strings(name = "sex", start_idx = 2, end_idx = 3)
agegroup_categories_dt <- RcensusPkg::get_category_strings(name = "agegroup", start_idx = 2, end_idx = 19)

# 4. Add the "SEX" and "AGEGROUP" variables from integer values to their string label equivalents:
# Join the SEX
data.table::setkey(ut_dt, SEX)
data.table::setkey(sex_categories_dt, val)

ut_labelled_dt <- ut_dt[sex_categories_dt]

# Join the AGEGROUP
data.table::setkey(ut_labelled_dt, AGEGROUP)
data.table::setkey(agegroup_categories_dt, val)

ut_labelled_dt <- ut_labelled_dt[agegroup_categories_dt]
