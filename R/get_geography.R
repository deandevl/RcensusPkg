#' @title get_geography
#'
#' @description Get the list of geography entities available (state, county, tract, etc)
#' for a specific dataset.
#'
#' Function produces a \code{data.table} of all the geography "name" and "geoLevelDisplay" variables
#'   available for a specific dataset and optionally a vintage.
#'
#' @param dataset A required string that sets the acronym name of the data set of interest (e.g. "acs/acs5")
#' @param vintage An optional numeric that sets the year of interest.
#'
#' @return A \code{data.table}
#'
#' @examples
#' library(jsonlite)
#' library(data.table)
#' library(httr)
#' library(RcensusPkg)

#' # Get the geographies available for dataset "acs/acs1/profile" with vintage 2019
#' acs1_profile_geo_dt <- RcensusPkg::get_geography(
#'   dataset = "acs/acs1/profile",
#'   vintage = 2019
#' )
#'
#' @import data.table
#' @import httr
#' @import jsonlite
#'
#' @export
get_geography <- function(dataset, vintage = NULL){

  # Create a string url based on the submitted parameters
  a_url <- .get_url(dataset, vintage)

  a_url <- paste(a_url, "geography.json", sep="/")

  # Make a web request
  resp <- httr::GET(a_url)

  # Check the response as valid JSON
  .check_response(resp)

  # Parse the response and return raw JSON
  raw_json <- .parse_response(resp)

  # Return a data.table
  geo_dt <- data.table(
    name = raw_json$fips$name,
    geoLevelDisplay = raw_json$fips$geoLevelDisplay
  )

  return(geo_dt)
}
