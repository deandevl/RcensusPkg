library(jsonlite)
library(data.table)
library(httr)
library(RcensusPkg)

# The following shows how the "predicates" parameter of RcensusPkg::get_vintage_data()
#   can be applied. 
# See Census Data API User Guide (https://www.census.gov/data/developers/guidance.html) for
#  forming predicates.  
# The following example is from the guide under the Predicate section.

# Let us first check the variables for this dataset and vintage
state_pop_variables_dt <- RcensusPkg::get_variable_names(
  dataset = "pep/natstprc",
  vintage = 2014
)

# Notice that the variable "DATE_" in the "required" column has a value of "true". This means that in
#  all of our requests to the API (i.e. RcensusPkg::get_vintage_data())
#  we must include this variable in our "vars" vector parameter or 
#  is included in one of our "predicates" phrases. In our call below
#  "DATE_" is in a predicate phrase, so we're good.

# This example includes just one predicate
state_pop_dt <- RcensusPkg::get_vintage_data(
  dataset = "pep/natstprc",
  vintage = 2014,
  vars = c("STNAME", "POP"),
  region = "state:*",
  predicates = "&DATE_=7",
  NAME_GEOID = FALSE
)
# Returned are total populations for 50 states plus Washington DC and Puerto Rico

# This next example is also from the guide and has multiple predicate phrases:
#   &PORT=0101 is Portland, ME
#   &PORT=0102 is Bangor, ME
# The dataset is "timeseries/intltrade/exports/porths" -- 
#   International Trade -- U.S. Exports by Port and Harmonized System
exports_dt <- RcensusPkg::get_vintage_data(
  dataset = "timeseries/intltrade/exports/porths",
  vars = c("PORT_NAME", "ALL_VAL_YR"),
  predicates = c("&time=2013-01", "&PORT=0101", "&PORT=0102"),
  NAME_GEOID = FALSE
)