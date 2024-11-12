#' remove_area_water
#'
#'  Function removes water area geometries from downloaded Census Bureau shapefiles
#'
#' @param x An sf shapefile object with possible areas of water downloaded from the Census Bureau
#' @param vintage An integer that specifies the year of the shapefile. The default is 2020.
#' @param output_dir A full directory path where supportive shapefiles and their associated files will be downloaded.
#'   The default is the directory defined by the value returned by \code{tempdir()}.
#'
#' @importFrom sf st_transform
#' @importFrom sf st_filter
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_difference
#' @importFrom sf st_union
#' @importFrom purrr map
#' @importFrom data.table rbindlist
#'
#' @return An sf shapefile object with the water area geometries removed.
#'
#' @author Rick Dean
#'
#' @export
remove_area_water <- function(
    x,
    vintage = 2020,
    output_dir = tempdir(check = T)){

  if (!"sf" %in% class(x)) {
    stop("The input dataset is not an sf object.")
  }

  # Get the US counties shapefile
  us_counties_sf <- RcensusPkg::tiger_counties_sf(
    output_dir = output_dir,
    vintage = vintage,
    general = TRUE,
    sf_info = F
  )

  # Identify the counties that overlap the input sf object
  filtered_county_sf <- us_counties_sf |>
    sf::st_transform(sf::st_crs(x)) |>
    sf::st_filter(x)

  # If us_county_sf has no observations then exit
  if (nrow(filtered_county_sf) == 0) {
    stop("Your shapefile geometries do not appear to be in the United States.")
  }

  # Get a vector of GEOIDs
  county_GEOID_v <- filtered_county_sf$GEOID
  get_area_water_fun <- function(geoid){
    RcensusPkg::tiger_water_sf(
      state = substr(geoid, 1, 2),
      county = substr(geoid, 3, 5),
      vintage = vintage,
      output_dir = output_dir,
      sf_info = F
    )
  }

  # Download the area water shapefile geometries for the above GEOID's
  area_water_lst <- purrr::map(county_GEOID_v, get_area_water_fun)
  area_water_dt <- data.table::rbindlist(area_water_lst)
  area_water_sf <- sf::st_as_sf(area_water_dt) |>
    sf::st_transform(sf::st_crs(x)) |>
    sf::st_filter(x)

  x_without_water_sf <- sf::st_difference(x, sf::st_union(area_water_sf))

  return(x_without_water_sf)
}
