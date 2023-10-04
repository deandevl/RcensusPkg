library(httr)
library(here)
library(sf)
library(data.table)
library(magrittr)
library(usmap)
library(purrr)
library(ggplot2)
library(RspatialPkg)
library(RplotterPkg)
library(RcensusPkg)

years <- c(1990, 2000, 2010, 2020)
set_crs <- c(4326, 4326, 4326, 4326)
vars <- c("CO", "COUNTY", "COUNTY", "COUNTYFP")
tx_tarrant_fips <- usmap::fips(state = "texas", county = "tarrant")
tx_fips <- substr(tx_tarrant_fips, 1, 2)
tarrant_fips <- substr(tx_tarrant_fips, 3, 5)

output_dir <- file.path(here(), "demo", "shapefiles")

build_plot <- function(id, years, set_crs, vars, state_fips, county_fips){
  tx_tracts_sf <-  RcensusPkg::tiger_tracts_sf(
    state = state_fips,
    vintage = years[[id]],
    general = T,
    set_crs = set_crs[[id]],
    output_dir = output_dir,
    sf_info = F
  )

  col_name <- vars[[id]]
  tarrant_tracts_sf <- data.table::as.data.table(tx_tracts_sf) %>%
    .[.[[col_name]] == county_fips, ] %>%
    sf::st_as_sf(.)

  tarrant_tracts_plot <- RspatialPkg::get_geom_sf(
    sf = tarrant_tracts_sf,
    subtitle = paste0(years[[id]], ": ", nrow(tarrant_tracts_sf), " tracts"),
    hide_x_tics = T,
    hide_y_tics = T
  )
  return(tarrant_tracts_plot)
}

plot_lst <- purrr::map(
  1:4,
  build_plot,
  years = years,
  set_crs = set_crs,
  vars = vars,
  state_fips = tx_fips,
  county_fips = tarrant_fips
)

names(plot_lst) <- years

layout <- list(
  plots = plot_lst,
  rows = c(1, 1, 2, 2),
  cols = c(1, 2, 1, 2)
)

RplotterPkg::multi_panel_grid(
  layout = layout,
  col_widths = c(5, 5),
  row_heights = c(5, 5)
)

