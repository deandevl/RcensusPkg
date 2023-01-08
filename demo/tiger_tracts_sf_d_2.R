library(httr)
library(here)
library(sf)
library(data.table)
library(magrittr)
library(usmap)
library(ggplot2)
library(RspatialPkg)
library(RcensusPkg)

years <- c(1990, 2000, 2010, 2020)
crs_transform <- c(3426, 3426, 3426, 3426)
vars <- c("CO", "COUNTY", "COUNTY", "COUNTYFP")
tx_tarrant_fips <- usmap::fips(state = "texas", county = "tarrant")
tx_fips <- substr(tx_tarrant_fips, 1, 2)
tarrant_fips <- substr(tx_tarrant_fips, 3, 5)

build_plot <- function(id, years, crs_transform, vars, state_fips, county_fips){
  output_dir <- file.path(here(), "demo", "shapefiles")

  tx_tracts_sf <-  RcensusPkg::tiger_tracts_sf(
    state = state_fips,
    vintage = years[[id]],
    output_dir = output_dir,
    general = T,
    crs_transform = crs_transform[[id]]
  )

  col_name <- vars[[id]]
  tarrant_tracts_sf <- data.table::as.data.table(tx_tracts_sf) %>%
    .[.[[col_name]] == county_fips, ] %>%
    sf::st_as_sf(.)

  tarrant_tracts_plot <- RspatialPkg::get_geom_sf(
    sf = tarrant_tracts_sf,
    title = paste0(years[[id]], ": ", nrow(tarrant_tracts_sf), " tracts"),
    hide_x_tics = T,
    hide_y_tics = T
  )
  return(tarrant_tracts_plot)
}

plot_lst <- purrr::map(
  1:4,
  build_plot,
  years = years,
  crs_transform = crs_transform,
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

