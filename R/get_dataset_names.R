#' get_dataset_names
#'
#' Get the acronym names and descriptions of the Census Bureau's datasets.
#'
#' Function produces a data.table/dataframe of the
#'   Census Bureau's dataset acronym names that can be used in other \code{Rcensus::}
#'   functions calling for a dataset acronym name. See Census Bureau's publicly available
#'    \href{https://www.census.gov/data/developers/data-sets.html}{datasets} for
#'    descriptions.
#'
#' @param vintage A numeric for the year to select the datasets. If NULL, then all the years are returned.
#' @param filter_name_str A character string by which to filter the resultant data.table using the
#'    "name" column.
#' @param filter_title_str A character string by which to filter the resultant data.table using the
#'    "title" column.
#' @param ignore_case A logical which if FALSE will not ignore case in filtering the "title" column.
#' @param brief A logical which if TRUE will return a resultant data.table with just columns
#'    "name", "vintage", "title". The default is TRUE.
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
get_dataset_names <- function(
  vintage = NULL,
  filter_name_str = NULL,
  filter_title_str = NULL,
  ignore_case = TRUE,
  brief = TRUE){

  year <- vintage

  # Create the url
  a_url <- "https://api.census.gov/data.json"

  # Make a web request
  resp <- httr::GET(a_url)

  # Check the response as valid JSON
  check <- .check_response(resp)

  # Parse the response and return raw JSON
  raw_json <- .parse_response(resp)

  datasets_df <- jsonlite::flatten(raw_json[["dataset"]])
  colnames(datasets_df) <- gsub("c_","",colnames(datasets_df))

  datasets_dt <- data.table::as.data.table(datasets_df)

  change_name <- function(x){
    paste(x[["dataset"]], collapse = "/")
  }
  change_url <- function(x){
    return(x[["distribution"]][["accessURL"]])
  }

  datasets_dt[, name := apply(datasets_dt, 1, change_name)]
  datasets_dt[, url := apply(datasets_dt, 1, change_url)]

  select_cols <- c("name","vintage","title","url","isTimeseries","description","modified")
  if(brief){
    select_cols <- c("name","vintage","title")
  }
  datasets_dt <- datasets_dt[, ..select_cols]

  if(!is.null(year)){
    datasets_dt <-  datasets_dt[vintage == year,]
  }

  if(!is.null(filter_name_str)){
    datasets_dt <- datasets_dt[grepl(filter_name_str, datasets_dt$name, ignore.case = ignore_case, fixed = FALSE)]
  }
  if(!is.null(filter_title_str)){
    datasets_dt <- datasets_dt[grepl(filter_title_str, datasets_dt$title, ignore.case = ignore_case, fixed = FALSE)]
  }

  data.table::setorderv(datasets_dt, cols = c("name", "vintage"))

  return(datasets_dt)
}
