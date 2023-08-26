#' .send_tiger_url
#'
#'  An internal function for sending http requests to
#'   the US Census Bureau TIGER api via httr::GET() function.
#'
#' @param a_url A required string that defines the url to be sent in the http GET request.
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#' @param crs_transform A numeric which if non-NULL calls sf::st_transform()
#'   to perform a crs transform of the geometries.
#' @param sf_info A logical which if TRUE displays info on the resulting simple feature object.
#' @param do_progress A logical which if TRUE displays a progress bar during the download.
#' @param caller A string that identifies the function making the call.
#'
#' @importFrom httr write_disk
#' @importFrom httr stop_for_status
#' @importFrom httr GET
#' @importFrom utils unzip
#' @importFrom sf st_read
#'
#' @return A data frame object of class sf, data frame
#'
#' @author Rick Dean
#'
#' @keywords internal
.send_tiger_url <- function(
    a_url = NULL,
    output_dir = NULL,
    crs_transform = NULL,
    sf_info = TRUE,
    do_progress = FALSE,
    caller = NULL
) {
  if(is.null(output_dir)){
    stop(paste0("The ouput directory ", output_dir ," does not exist. Please create this directory."))
  }

  zip_file_name <- base::basename(a_url)

  files_zip_path <- file.path(output_dir, zip_file_name)

  census_response <- NULL
  if(do_progress){
    census_response <- httr::GET(
      a_url,
      httr::write_disk(files_zip_path, overwrite = TRUE),
      progress()
    )
  }else {
    census_response <- httr::GET(
      a_url,
      httr::write_disk(files_zip_path, overwrite = TRUE)
    )
  }

  httr::stop_for_status(census_response, task = paste0("Request called from ", caller))

  utils::unzip(files_zip_path, exdir = output_dir, overwrite = TRUE)

  #shape_file_path <- list.files(path = output_dir, full.names = TRUE, recursive = TRUE, pattern = "\\.shp$")
  #tiger_sf <- sf::st_read(dsn = shape_file_path)
  tiger_sf <- sf::st_read(dsn = output_dir, quiet = !sf_info)
  if(!is.null(crs_transform)){
    sf::st_crs(tiger_sf) <- crs_transform
    sf::st_transform(tiger_sf, crs = crs_transform)
  }

  return(tiger_sf)
}
