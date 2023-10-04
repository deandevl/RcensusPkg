
#' get_variable_names
#'
#' Get Census Bureau variable acronym names and their label descriptions.
#'
#' Function produces a data.table of variable acronym names and their
#'    descriptions from the Census Bureau's publicly available
#'    \href{https://www.census.gov/data/developers/data-sets.html}{datasets}.
#'    The function returns 4 columns:
#'    \describe{
#'      \item{name}{the name of the parameter}
#'      \item{label}{the Burau's description of the parameter}
#'      \item{required}{a boolean indicating if the parameter is required}
#'      \item{predicateType}{a string indicating the variable primitive type}
#'    }
#'
#'    Note that a variable with a "required" character value "true" must be included
#'    in your data requests (i.e. \code{RcensusPkg::get_vintage_data()}) or
#'    the API will return an error.
#'
#' @param dataset A required string that sets the name of the dataset of interest (e.g. "acs/acs5").
#'   See \code{Rcensus::get_dataset_names()} for available dataset names.
#' @param vintage An required numeric that sets the year of interest.
#' @param vars An optional vector of variable names whose descriptions are of interest.
#' @param group An optional string that sets the group name associated with a set of variables.
#'   See \code{Rcensus::get_groups()} for available group names under a specific dataset and vintage.
#' @param filter_group_est A logical which if TRUE will filter the variable names from \code{group} and return
#'   only estimate related variable names. The default is FALSE.
#' @param filter_name_str A character string by which to filter the resultant data.table's "name" column.
#' @param filter_label_str A character string by which to filter the resultant data.table's "label" column.
#' @param filter_concept_str A character string by which to filter the resultant data.table's "concept" column.
#' @param ignore_case A logical which if FALSE will not ignore case in filtering the "name", "label", "concept" column.
#' @param fixed A logical which if TRUE, then the above filter strings are used 'as is' in matching.
#'
#' @import data.table
#' @import httr
#' @import jsonlite
#' @importFrom purrr map2
#'
#' @return A data.table
#'
#' @author Rick Dean
#'
#' @export
get_variable_names <- function(
  dataset = NULL,
  vintage = NULL,
  vars = NULL,
  group = NULL,
  filter_group_est = FALSE,
  filter_name_str = NULL,
  filter_label_str = NULL,
  filter_concept_str = NULL,
  ignore_case = TRUE,
  fixed = FALSE){

  if(is.null(dataset)){
    stop("A dataset is required for get_variable_names()")
  }

  if(is.null(vintage)){
    stop("A vintage is required for get_variable_names()")
  }

  add_variable <- function(name, var) {
    concept <- NA
    if(!is.null(var$concept)){
      concept <- var$concept
    }
    required <- NA
    if(!is.null(var$required)){
      required <- var$required
    }
    predicateType = NA
    if(length(var$predicateType) > 0){
      predicateType <- var$predicateType
    }
    list(
      name = name,
      label = var$label,
      concept = concept,
      required = required,
      predicateType = predicateType
    )
  }

  a_url <- .get_url(dataset, vintage)

  if(!is.null(group)){
    a_url <- paste0(a_url, "/groups/", group, ".json")
  }else {
    a_url <- paste(a_url, "variables.json", sep = "/")
  }

  # Make a web request
  resp <- httr::GET(a_url)

  # Check the response as valid JSON
  check <- .check_response(resp)

  # Parse the response and return raw JSON
  raw_json <- .parse_response(resp)

  # Check and add variables
  variable_lst <- purrr::map2(names(raw_json$variables), raw_json$variables,  add_variable)

  dt <- data.table::rbindlist(variable_lst, fill = T)

  # If variables were derived by group, do we filter their names
  #   to get just estimates and margin of error related variable names
  if(!is.null(group) & filter_group_est){
    dt <- dt[endsWith(name, "E"),]
  }

  # Order by name
  data.table::setorder(dt, name)

  # Look for specific variables?
  if(!is.null(vars)){
    dt <- dt[name %in% vars,]
  }

  # Filtering of "name" and/or "label", "concept" columns?
  if(!is.null(filter_name_str)){
    dt <- dt[grepl(filter_name_str, dt$name, ignore.case = ignore_case, fixed = fixed)]
  }
  if(!is.null(filter_label_str)){
    dt <- dt[grepl(filter_label_str, dt$label, ignore.case = ignore_case, fixed = fixed)]
  }
  if(!is.null(filter_concept_str)){
    dt <- dt[grepl(filter_concept_str, dt$concept, ignore.case = ignore_case, fixed = fixed)]
  }
  return(dt)
}
