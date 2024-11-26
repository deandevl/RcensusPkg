library(sf)
library(here)
library(magrittr)
library(ggplot2)
library(RColorBrewer)
library(RspatialPkg)
library(RplotterPkg)
library(RcensusPkg)

output_dir <- file.path(here(), "demo", "shapefiles")

# --------------------plot just the states without joining data (the default case)----------------
RcensusPkg::plot_us_data(
  title = "A Default Mapping of US States",
  output_dir = output_dir
)

# ----------------------------continuous case-------------------------
# Get the median age for each state (i.e. "B01002_001E"):
 states_median_age_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1",
  vintage = 2019,
  vars = "B01002_001E",
  region = "state"
) %>%
  data.table::setnames(., old = "B01002_001E", new = "median_age") %>%
  .[, median_age := as.numeric(median_age)]

# this is used for debugging--an obvious value at the lower end of the scale
#states_median_age_dt[NAME == "Alaska", median_age := 30]

RcensusPkg::plot_us_data(
  df = states_median_age_dt,
  #df = states_median_age_dt[NAME %in% c("Ohio","Indiana","West Virginia","Kentucky"),],
  title = "Median Age in US",
  states_col = "NAME",
  value_col = "median_age",
  output_dir = output_dir,
  scale_breaks = seq(30,50,5),
  scale_limits = c(30,50),
  scale_labels = c("Age:30","Age:35","Age:40","Age:45","Age:50")
)

# ------------------discrete case-----------------
party_df <- data.frame(
  states = c("ohio","indiana","west virginia","kentucky"),
  party = as.factor(c("Republican","Democrate","Democrate","Republican"))
)

RcensusPkg::plot_us_data(
  df = party_df,
  title = "Party Affiliation",
  states_col = "states",
  value_col = "party",
  scale_limits = c("Republican","Democrate"),
  scale_values = c("red","blue"),
  scale_breaks = c("Republican","Democrate"),
  output_dir = output_dir,
  text_col = "states",
  text_color = "white",
  text_size = 4,
  text_fontface = "bold",
  sf_alpha = 0.5,
  sf_linewidth = 1,
  scale_labels = c("Republican","Democrate"),
  legend_pos = "top"
)

# ------------------------another discrete------------------------
data_file_path <- file.path(here(), "demo", "data", "us_vote_2020.csv")
vote2020_dt <- data.table::fread(file = data_file_path)
names(vote2020_dt)

# Change the name of column "state" to "NAME" and set the "called" column as a factor:
vote2020_dt[, `:=`(called = as.factor(called))] %>%
  data.table::setnames(old = "called", new = "Party")

RcensusPkg::plot_us_data(
  df = vote2020_dt,
  title = "US Presidential Vote 2020",
  states_col = "state",
  value_col = "Party",
  output_dir = output_dir,
  scale_breaks = c("R","D"),
  scale_limits = c("R","D"),
  scale_values = c("red","blue"),
  scale_labels = c("R","D"),
  sf_color = "white"
)

# --------------------------US house values with user defined scaling-------------
# Get housing values data
housing_values_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1",
  vintage = 2019,
  vars = "B25077_001E",
  region = "state:*"
) %>%
  data.table::setnames(old = "B25077_001E", new = "estimate") %>%
  .[, estimate := as.numeric(estimate)] %>%
  data.table::setnames(., old = "estimate", new = "Median_House_Values")

# Define "pretty" intervals for housing values
intervals <- classInt::classIntervals(
  housing_values_dt$Median_House_Values,
  n = 6,
  style = "pretty"
)
breaks <- intervals$brks
labels <- c("$100,000","$200,000","$300,000","$400,000","$500,000","$600,000","$700,000")

# Plot the map of US housing values
RcensusPkg::plot_us_data(
  df = housing_values_dt,
  title = "US Housing Values",
  states_col = "NAME",
  value_col = "Median_House_Values",
  output_dir = output_dir,
  scale_breaks = breaks,
  scale_limits = c(100000, 700000),
  scale_labels = labels,
  scale_colors = RColorBrewer::brewer.pal(8,"YlOrRd"),
  show_legend = TRUE,
  legend_pos = "top",
  legend_key_width = 2.0
)

# -----------------------------multiplot------------------------------
# Data is percent of computers present across states
# Comparing 2013 with 2023 maps

# 2013 data
acs1_computers_data_2013_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2013,
  vars = c("DP02_0150E", "DP02_0151PE", "DP02_0152PE"),
  region = "state:*"
)
acs1_computers_data_2013_dt <- acs1_computers_data_2013_dt %>%
  data.table::setnames(
    old = c("NAME", "DP02_0150E", "DP02_0151PE", "DP02_0152PE"),
    new = c("State", "Total", "ComputerPresent", "BroadbandPresent")) %>%
  .[, .(GEOID, State, Total, ComputerPresent, BroadbandPresent)] %>%
  .[, `:=`(Total = as.numeric(Total), ComputerPresent = as.numeric(ComputerPresent),
           BroadbandPresent = as.numeric(BroadbandPresent))] %>%
  .[order(State)]

# 2023 data
acs1_computers_data_2023_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2023,
  vars = c("DP02_0152E", "DP02_0153PE", "DP02_0154PE"),
  region = "state:*"
)
acs1_computers_data_2023_dt <- acs1_computers_data_2023_dt %>%
  data.table::setnames(
    old = c("NAME", "DP02_0152E", "DP02_0153PE", "DP02_0154PE"),
    new = c("State", "Total", "ComputerPresent", "BroadbandPresent")) %>%
  .[, `:=`(Total = as.numeric(Total), ComputerPresent = as.numeric(ComputerPresent),
           BroadbandPresent = as.numeric(BroadbandPresent))]

# Create the plots for 2013, 2021
computers_2013_lst <- RcensusPkg::plot_us_data(
  df = acs1_computers_data_2013_dt[!(State %in% c("Alaska","Hawaii","Puerto Rico")),],
  states_col = "State",
  value_col = "ComputerPresent",
  output_dir = output_dir,
  title = "Computers in 2013",
  scale_breaks = seq(70,100,5),
  scale_limits = c(70,100),
  scale_colors = RColorBrewer::brewer.pal(8, "GnBu"),
  display_plot = FALSE,
  legend_key_width = 1.0
)
computers_2013_lst$plots$lower_48

computers_2023_lst <- RcensusPkg::plot_us_data(
  df = acs1_computers_data_2023_dt[!(State %in% c("Alaska","Hawaii","Puerto Rico")),],
  states_col = "State",
  value_col = "ComputerPresent",
  output_dir = output_dir,
  title = "Computers in 2023",
  scale_breaks = seq(70,100,5),
  scale_limits = c(70,100),
  scale_colors = RColorBrewer::brewer.pal(8, "GnBu"),
  display_plot = FALSE,
  legend_key_width = 1.0
)
computers_2023_lst$plots$lower_48

plot_lst <- list(computers_2013_lst$plots$lower_48, computers_2023_lst$plots$lower_48)

layout <- list(
  plots = plot_lst,
  rows = c(1, 1),
  cols = c(1, 2)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  cell_width = 20,
  cell_height = 12,
  title = "Computers Present 2013 - 2023",
  plot_titles = c("Year: 2013","Year:2023")
)
