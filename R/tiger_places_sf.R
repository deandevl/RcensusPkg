#' tiger_places_sf
#'
#' This function performs three tasks:
#' \enumerate{
#'   \item Download to a temporary directory a zip file from the TIGER/Line Shapefiles database.
#'   \item Unzip the zip file and locate the shape file of interest.
#'   \item Read and convert the shape file to a simple feature object.
#' }
#'
#' Returns simple feature (sf) places related geometric polygons provided
#'   by the US Census Bureau's TIGER/Line Shapefiles database. See
#'   \href{https://r-spatial.github.io/sf/articles/sf1.html}{Simple Features for R}
#'   for more information on simple features. Along with the geometries, additional places related
#'   variables are provided.  See
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_F-R.pdf}{Appendix I-5. Record Layout: Place State-based Shapefile}
#'   for a description of place related variables of the sf file.
#'   For further information on the Census Bureau's shape files see
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2021/TGRSHP2021_TechDoc_Ch3.pdf}{About the 2021 TIGER/Line Shapefiles}.
#'   From \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_Ch4.pdf}{4.14 Places – Incorporated Places and Census Designated Places} --
#'
#'   "Incorporated Places: An incorporated place provides governmental functions
#'   for a concentration of people. Incorporated places may extend across county and county subdivision
#'   boundaries, but never across state boundaries. An incorporated place usually is a city, town, village, or
#'   borough, but can have other legal descriptions."
#'
#'   "Census Designated Places (CDPs): CDPs are the statistical counterparts of incorporated places. CDPs are settled concentrations of
#'   population that are identifiable by name but not legally incorporated under the laws of the state in which
#'   the CDPs are located."
#'
#' The function returns the simple feature object which can easily be mapped (see \href{https://github.com/deandevl/RspatialPkg}{RspatialPkg::get_geom_sf()}) or
#' joined with US Census Bureau demographic data via the GEOID value.
#'
#' Some earlier vintages may have NA for the crs so you may need to specify the \code{crs_transform} to 3426.  Also
#'   you may be interested in using a state level crs.  See \href{https://epsg.io/}{epsg.io} to search worldwide for crs.
#'
#' @param state The two-digit FIPS code for the state of interest. This is required parameter.
#'  See \href{https://cran.r-project.org/web/packages/usmap/usmap.pdf}{usmap::fips function} for finding FIPS codes.
#' @param output_dir A full directory path where the shapefile will be downloaded. This is a required parameter. The function will stop
#' if this directory does not exist.  Be aware that all files in this directory are removed before downloading.
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#'   The value should be greater than 2010.
#' @param general A logical which if TRUE will download a less detailed, more gerneralized version of the places geometries.
#' @param crs_transform A numeric or string that if non-NULL transforms the geometries to this coordinate reference system. See
#'   \href{sf::st_transform()}{https://cran.r-project.org/web/packages/sf/sf.pdf} for acceptable values.
#' @param sf_info A logical which if TRUE displays info on the resulting simple feature object.     
#' @param do_progress A logical which if TRUE displays a progress bar during the download.    
#' @param shapefile A full file path to a shapefile folder with its unzipped files to be processed instead of downloading.
#' @param express A logical expression object used to filter the resultant simple feature dataframe. 
#'   For example, one of the columns of the resultant simple feature dataframe is "NAME" for major places.
#'   Say we wanted to return just the geometries for major places in Kentucky such as "Bowling Green" and "Louisville".
#'   We would assign \code{express} equal to: expression(NAME %in% c("Bowling Green", "Louisville" )). The expression will be 
#'   evaluated and only the geometries for the two places will be returned.
#' @param check_na A logical which if TRUE will remove rows that have missing values for any of the columns.
#'   The default is to not check the columns for NA values.    
#'
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom sf st_as_sf
#' @importFrom data.table as.data.table
#'
#' @return A data frame object of class sf, data frame
#'
#' @author Rick Dean
#'
#' @export
tiger_places_sf <- function(
  state = NULL,
  output_dir = NULL,
  vintage = 2020,
  general = FALSE,
  crs_transform = NULL,
  sf_info = TRUE,
  do_progress = FALSE,
  shapefile = NULL,
  express = NULL,
  check_na = FALSE
){
  if(is.null(shapefile) & is.null(state)){
    stop("The state argument is required")
  }

  if(is.null(shapefile) & vintage < 2011){
    stop("Vintage is not currently available for years prior to 2011")
  }

  if(!is.null(shapefile)){ # Reading shapefile
    if(!file.exists(shapefile)){
      stop(paste0("Shapefile folder ", shapefile, " does not exists."))
    }
    tiger_sf <- sf::st_read(dsn = shapefile)
    if(!is.null(crs_transform)){
      sf::st_transform(tiger_sf, crs = crs_transform)
    }
    return(tiger_sf)
  }else { # Downloading shapefile
    vintage_char <- as.character(vintage)
    a_url <- NULL

    if(general){
      a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_place_500k.zip",
                   vintage_char, vintage_char, state)
    }else {
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/PLACE/tl_%s_%s_place.zip",
                   vintage_char, vintage_char, state)
    }
    tiger_sf <- .send_tiger_url(
      a_url = a_url, 
      output_dir = output_dir, 
      crs_transform = crs_transform, 
      sf_info = sf_info,
      do_progress = do_progress,
      caller = "tiger_places_sf"
    )
    
    if(!is.null(express)){
      tiger_dt <- data.table::as.data.table(tiger_sf)
      tiger_dt <- tiger_dt[eval(express), ]
      tiger_sf <- sf::st_as_sf(tiger_dt)
    }
    
    if(check_na){
      tiger_sf <- na.omit(tiger_sf)
    }
    
    return(tiger_sf)
  }
}
