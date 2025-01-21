#' @title .check_response
#'
#' @description An internal function that checks the httr GET response.
#'
#' @param resp The httr response object.
#'
#' @return If the request is successful, returns \code{TRUE}, otherwise the
#'   program is stopped with a message.

#' @keywords internal
#'
.check_response <- function(resp){
  status <- resp[["status_code"]]

  if(!(status %in% c(200, 201, 202))){
    if(status == 404){
      stop("Invalid request, (404) not found.")
    }else if(status == 400){
      stop(
        paste0(
          "The Census Bureau returned the following error message:\n",
          resp[["error_message"]],
          "\n Your API call was: ", resp[["url"]]
        )
      )
    }else if(status == 204){
      stop(
        paste0(
          "204, No content was returned. \n",
          "Your API call was: ", resp[["url"]]
        )
      )
    }
  }else {
    return(TRUE)
  }
}
