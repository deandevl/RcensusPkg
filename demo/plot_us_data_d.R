library(sf)
library(here)
library(magrittr)
library(ggplot2)
library(ggplotify)
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

us_plot_lst <- RcensusPkg::plot_us_data(
  df = states_median_age_dt,
  #df = states_median_age_dt[NAME %in% c("Ohio","Indiana","West Virginia","Kentucky"),],
  states_col = "NAME",
  value_col = "median_age",
  output_dir = output_dir,
  scale_breaks = seq(30,50,5),
  scale_limits = c(30,50),
  scale_labels = c("Age:30","Age:35","Age:40","Age:45","Age:50"),
  display_plot = F
)
us_plot_lst[["us_states"]]

# ------------------discrete case-----------------
party_df <- data.frame(
  states = c("ohio","indiana","west virginia","kentucky"),
  party = as.factor(c("Republican","Democrate","Democrate","Republican"))
)

RcensusPkg::plot_us_data(
  df = party_df,
  states_col = "states",
  value_col = "party",
  scale_limits = c("Republican","Democrate"),
  scale_values = c("red","blue"),
  scale_breaks = c("Republican","Democrate"),
  output_dir = output_dir,
  text_col = "NAME",
  text_color = "white",
  text_size = 4,
  text_fontface = "bold",
  sf_alpha = 0.5,
  sf_linewidth = 1,
  scale_labels = c("Republican","Democrate")
)

# ------------------------another discrete------------------------
data_file_path <- file.path(here(), "demo", "data", "us_vote_2020.csv")
vote2020_dt <- data.table::fread(file = data_file_path)
names(vote2020_dt)

# Change the name of column "state" to "NAME" and set the "called" column as a factor:
vote2020_dt[, `:=`(called = as.factor(called))] %>%
  data.table::setnames(old = "called", new = "Party")

us_vote_lst <- RcensusPkg::plot_us_data(
  df = vote2020_dt,
  states_col = "state",
  value_col = "Party",
  output_dir = output_dir,
  scale_breaks = c("R","D"),
  scale_values = c("red","blue"),
  scale_labels = c("R","D"),
  sf_color = "white",
  display_plot = F
)
us_vote_lst[["us_states"]]

# -----------------------------multiplot------------------------------
# Data is percent of computers present across states
# Comparing 2013 with 2021 maps

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

# 2021 data
acs1_computers_data_2021_dt <- RcensusPkg::get_vintage_data(
  dataset = "acs/acs1/profile",
  vintage = 2021,
  vars = c("DP02_0152E", "DP02_0153PE", "DP02_0154PE"),
  region = "state:*"
)
acs1_computers_data_2021_dt <- acs1_computers_data_2021_dt %>%
  data.table::setnames(
    old = c("NAME", "DP02_0152E", "DP02_0153PE", "DP02_0154PE"),
    new = c("State", "Total", "ComputerPresent", "BroadbandPresent")) %>%
  .[, `:=`(Total = as.numeric(Total), ComputerPresent = as.numeric(ComputerPresent),
           BroadbandPresent = as.numeric(BroadbandPresent))]

# Create the plots for 2013, 2021
computers_2013_lst <- RcensusPkg::plot_us_data(
 # df = acs1_computers_data_2013_dt[!(State %in% c("Alaska","Hawaii","Puerto Rico")),],
  df = acs1_computers_data_2013_dt,
  states_col = "State",
  value_col = "ComputerPresent",
  output_dir = output_dir,
  scale_breaks = seq(70,100,5),
  scale_limits = c(70,100),
  scale_colors = RColorBrewer::brewer.pal(8, "GnBu"),
  display_plot = FALSE
)
computers_2021_lst <- RcensusPkg::plot_us_data(
  df = acs1_computers_data_2021_dt[!(State %in% c("Alaska","Hawaii","Puerto Rico")),],
  states_col = "State",
  value_col = "ComputerPresent",
  output_dir = output_dir,
  scale_breaks = seq(70,100,5),
  scale_limits = c(70,100),
  scale_colors = RColorBrewer::brewer.pal(8, "GnBu"),
  display_plot = FALSE
)

# Layout the two maps vertically
plot_lst <- list(computers_2013_lst$us_states, computers_2021_lst$us_states)

layout <- list(
  plots = plot_lst,
  rows = c(1, 2),
  cols = c(1, 1)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  title = "Computers Present Across the US 2013/2021",
  plot_titles = c("Year: 2013", "Year: 2021"),
  cell_width = 16,
  cell_height = 10
)
