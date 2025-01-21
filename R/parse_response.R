#' @title .parse_response
#'
#' @description An internal function checks for valid json content
#' from the Bureau's response. The content is converted to R objects.
#'
#' @param resp The httr response object
#'
#' @importFrom jsonlite fromJSON
#'
#' @return Converted json content to R objects.
#'
#' @keywords internal
#'
.parse_response <- function(resp){
  content <- httr::content(resp, as="text")
  if(jsonlite::validate(content) == FALSE){
    stop(
      paste0(
        "The Census Bureau returned the following error message:",
        content
      )
    )
  }else{
    return(jsonlite::fromJSON(content))
  }
}
