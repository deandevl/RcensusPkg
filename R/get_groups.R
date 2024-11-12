#' get_groups
#'
#' Get the names of Census Bureau variable groups and their descriptive parameters
#'
#' Function produces a data.table of variable groups/tables and their descriptions.
#'
#' @param dataset A required string that sets the acronym name of the data set of interest (e.g. "acs/acs5")
#' @param vintage An required numeric that sets the year of interest.
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
get_groups <- function(
    dataset = NULL, 
    vintage = NULL) {

  if(is.null(dataset)){
    stop("A dataset acronym name is required for get_groups()")
  }
  
  if(is.null(vintage)){
    stop("A vintage is required for get_groups()")
  }

  # Create a string url based on the submitted parameters
  a_url <- .get_url(dataset, vintage)

  a_url <- paste(a_url, "groups.json", sep="/")

  # Make a web request
  resp <- httr::GET(a_url)

  # Check the response as valid JSON
  .check_response(resp)

   # Parse the response and return raw JSON
  raw_json <- .parse_response(resp)
  if(is.null(dim(raw_json$groups))){
    stop(paste0("Groups are not available for dataset ", dataset))
  }
  
  # Return a data.table
  dt <- data.table::as.data.table(raw_json[["groups"]])
  
  return(dt[, .(name, description, variables)])
}
