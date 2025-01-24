#' @title  .get_dt
#'
#' @description  An internal function for sending an R based http GET request
#'   to the US Census Bureau's Census Data API. The function returns a \code{data.table}
#'   with column values based on dataset, vintage, Census variable predicates, and
#'   geography.
#'
#'   See \href{https://www.census.gov/data/developers/guidance.html}{Census Data API User Guide} for
#'   definitions and examples in running an API query.
#'
#' @param a_url A string that sets the base url address. A calling function will call
#'  the \code{.get_url} function to define this parameter.
#' @param group A string that names an entire group of similar variables to be retrieved.
#'  For example the group value "B01001" would return values of all variables related to
#'  "SEX BY AGE".
#' @param vars A string vector of variable names to be acquired.
#' @param NAME_GEOID A logical which if \code{TRUE} will add "NAME" and "GEO_ID"
#'  variables to 'vars' string vector. The default is \code{TRUE}.
#' @param predicates A vector of strings that adds filtering to the list of variables in 'vars'.
#' @param region A string that specifies the geography of the request.
#' @param regionin A string that sets a qualifier for 'region'.
#' @param key All Census Bureau API requests require a client key from the calling function.
#'
#' @keywords internal
#'
.get_dt <- function(
    a_url = NULL,
    group = NULL,
    vars = NULL,
    NAME_GEOID = TRUE,
    predicates = NULL,
    region = NULL,
    regionin = NULL,
    key = NULL
){
  a_url <- paste0(a_url, "?get=")

  if(!is.null(group)){
    a_url <- paste0(a_url, "group(", group, ")")
  }

  if(!is.null(vars)){
    if(NAME_GEOID){
      vars <- c("NAME", "GEO_ID", vars)
    }
    get_vars <- paste(vars, collapse = ",")
    if(!is.null(group)){
      a_url = paste0(a_url, ",", get_vars)
    }else {
      a_url = paste0(a_url, get_vars)
    }
  }

  if(!is.null(predicates)){
    predicates_collapse <- paste(predicates, collapse = "")
    a_url <- paste0(a_url, predicates_collapse)
  }

  if(!is.null(region)){
    a_url <- paste0(a_url, "&for=", region)
  }
  if(!is.null(regionin)){
    a_url <- paste0(a_url, "&in=", regionin)
  }

  a_url <- paste0(a_url, "&key=", key)

  url_coded <- utils::URLencode(a_url)
  resp <- httr::GET(url = url_coded)

  # Check the response as valid JSON
  check <- .check_response(resp)

  # Parse the response and return raw JSON
  raw_json <- .parse_response(resp)

  # Create data,table
  dt <- data.table::as.data.table(raw_json)

  colnames(dt) <- raw_json[1,]
  dt <- dt[-1]

  return(dt)
}
