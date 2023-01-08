#' get_vintage_data
#'
#' Get Census Bureau data for a specific dataset, variables, and region
#'   in the form of a data.table.
#'
#' Function produces a data.table with selected Census Bureau variables as columns. The function requires an access key
#'   issued from the Bureau. Variables of interest can be specified individually or by group/table name.
#'   Predicate phrases can be specified for filtering the results.
#'
#' @param dataset A string that sets the name of the data set of interest (e.g. "acs/acs5").
#'  See Census Bureau's publicly available
#'  \href{https://www.census.gov/data/developers/data-sets.html}{datasets} for
#'  descriptions. Descriptions/vintages for datasets can also be found by running
#'  \code{Rcensus::get_dataset_names()}. This is a required parameter.
#' @param vintage An optional numeric that sets the vintage of interest. Available vintages
#'   for a specific dataset can be found by running \code{Rcensus::get_dataset_names()}.
#' @param vars A string vector of variable names to be acquired.
#'   Available variable names can be determined by running \code{Rcensus::get_variable_names()}.
#' @param NAME_GEOID A logical which if TRUE will add "NAME" and "GEO_ID" variables to \code{vars} string vector.
#'   The default is TRUE.
#' @param predicates An optional vector of strings that adds data filtering.
#'   See \href{https://www.census.gov/data/developers/guidance.html}{Census Data API User Guide} for
#'   forming predicates and filtering or limiting variables. As noted in the guide each predicate must
#'   start with an ampersand sign.
#' @param group An optional string that names an entire group of similar variables to be retrieved.
#'   For example the group value "B01001" would return values of all variables related to
#'  "SEX BY AGE". To find available groups submit a dataset and vintage to \code{RcensusPkg::get_groups}.
#' @param region An optional string that specifies the geography of the request.
#'   See \href{https://www.census.gov/data/developers/guidance.html}{Census Data API User Guide} for
#'   specifying this and the \code{regionin} parameter. Not all regions such as counties,
#'   blocks, or tracts are available for a specific dataset and vintage. Use
#'   \code{Rcensus::get_geography()} to check on both \code{region} and \code{regionin}.
#' @param regionin A string that sets a qualifier for \code{region}.
#' @param na_cols If TRUE will remove all rows with missing values. If a
#'   vector of column names/integers will check only those columns for missing values.
#' @param key A string that sets the access key. All Census Bureau API requests require an access key.
#'   Sign-up for a key is free and can be obtained \href{https://api.census.gov/data/key_signup.html}{here}.
#'   The function will check for a global setting of the key via \code{Sys.getenv("CENSUS_KEY")}.
#'   Run \code{usethis::edit_r_environ()} and edit your .Renviron file with the line: CENSUS_KEY=your key
#'   to create the global association. This is a required parameter.
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
get_vintage_data <- function(
  dataset = NULL,
  vintage = NULL,
  vars = NULL,
  NAME_GEOID = TRUE,
  predicates = NULL,
  group = NULL,
  region = NULL,
  regionin = NULL,
  na_cols = NULL,
  key = Sys.getenv("CENSUS_KEY")
){
  dt <- NULL

  # Check for key in environment
  key_from_env <- Sys.getenv("CENSUS_KEY")
  if(key_from_env == "" & key == key_from_env){
    stop("'key' argument is missing. A Census Bureau key is required.")
  }

  if(is.null(dataset)){
    stop("The parameter 'dataset' must be specified.")
  }
  if(is.null(group) & is.null(vars)){
    stop("The parameter 'vars' or 'group' must be specified")
  }

  # Create a string url based on the submitted parameters
  a_url <- .get_url(dataset, vintage)

  # Get the data.table
  dt <- .get_dt(
    a_url = a_url,
    group = group,
    vars = vars,
    NAME_GEOID = NAME_GEOID,
    predicates = predicates,
    region = region,
    regionin = regionin,
    key = key
  )

  if("GEO_ID" %in% names(dt)){
    dt[, c("pre", "GEOID") := tstrsplit(GEO_ID, "US")]
    dt[,`:=`(pre = NULL, GEO_ID = NULL)]
  }

  if(!is.null(group)){
    # Get the "long" forms of estimate and moe columns
    dt <- RcensusPkg::wide_to_long(dt = dt, do_est_moe = T)

    # The -555555555 values in "moe" column need to be defined as NA
    dt[, moe := ifelse(moe == -555555555, NA, moe)]
  }


  if(!is.null(na_cols)){
    if(is.logical(na_cols)){
      dt <- na.omit(dt)
    }else if(is.vector(na_cols)){
      dt <- na.omit(dt, cols = na_cols)
    }
  }

  return(dt)
}

