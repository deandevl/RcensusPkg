#' get_multi_vintage_data
#'
#' Get Census Bureau data for a specific dataset, variables, and region
#'   in the form of a dataframe for multiple vintages.
#'
#' Function produces a data.table/dataframe of
#'   Census Bureau data for multiple vintages.  The function requires an access key
#'   issued from the Bureau.
#'
#' @param dataset A string that sets the name of the data set of interest (e.g. "acs/acs5"). This is a required parameter.
#' @param vintage_v A numeric vector that sets the vintages of interest. This is a required parameter.
#' @param vars A string vector of variable acronym names to be acquired (e.g. "B15002_015").
#'   See RcensusPkg::get_variable_names() for obtaining acronym names.  This is a required parameter.
#' @param region A string that specifies the geography of the request. See \code{Rcensus::get_geography()} for
#'   assistance in obtaining these values.
#' @param regionin A string that sets a qualifier for \code{region}.
#' @param key A string that sets the access key. All Census Bureau API requests require an access key.
#'   Sign-up for a key is free and can be obtained \href{https://api.census.gov/data/key_signup.html}{here}.
#'   The function will check for a global setting of the key via \code{Sys.getenv("CENSUS_KEY")}.
#'   This is a required parameter.
#'
#' @import data.table
#' @import httr
#' @import jsonlite
#'
#' @return A data.table
#'
#' @author Rick Dean
#'
#' @export
get_multi_vintage_data <- function(
  dataset = NULL,
  vintage_v = NULL,
  vars = NULL,
  region = NULL,
  regionin = NULL,
  key = Sys.getenv("CENSUS_KEY")
) {
  # Check for key in environment
  key_from_env <- Sys.getenv("CENSUS_KEY")
  if(key_from_env == "" & key == key_from_env){
    stop("'key' argument is missing. A Census Bureau key is required.")
  }

  if(is.null(dataset)){
    stop("The parameter 'dataset' must be specified.")
  }
  if(is.null(vintage_v)){
    stop("The parameter 'vintage_v' must be specified")
  }
  if(is.null(vars)){
    stop("The parameter 'vars' must be specified")
  }

  getDT <- function(vintage){
    # Create a string url based on the submitted parameters
    a_url <- .get_url(dataset, vintage)

    # Get the data.table
    dt <- .get_dt(
      a_url = a_url,
      key = key,
      vars = vars,
      region = region,
      regionin = regionin
    )

    if("GEO_ID" %in% names(dt)){
      dt[, c("pre", "GEOID") := tstrsplit(GEO_ID, "US")]
      dt[,`:=`(pre = NULL, GEO_ID = NULL)]
    }
    return(dt)
  }

  dt <- getDT(vintage_v[[1]])

  dt[, vintage := vintage_v[[1]]]

  for(i in 2:length(vintage_v)){
    a_dt <- getDT(vintage_v[[i]])

    a_dt[, vintage := vintage_v[[i]]]

    dt <- rbind(dt, a_dt)
  }

  return(dt)
}
