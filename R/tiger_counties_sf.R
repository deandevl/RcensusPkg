#' tiger_counties_sf
#'
#' This function performs three tasks:
#' \enumerate{
#'   \item Download to a temporary directory a zip file from the TIGER/Line Shapefiles database.
#'   \item Unzip the zip file and locate the shape file of interest.
#'   \item Read and convert the shape file to a simple feature object.
#' }
#'
#' Returns simple feature (sf) of the entire US county boundary related geometric polygons,
#'   provided by the US Census Bureau's TIGER/Line Shapefiles database. See
#'   \href{https://r-spatial.github.io/sf/articles/sf1.html}{Simple Features for R}
#'   for more information on simple features. Along with the geometries, additional county related
#'   variables are provided.  See \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_F-R.pdf}{Appendix I-3. Record Layout: County Subdivision State-based Shapefile)}
#'   for a description of county related variables of the sf file. For further information on the Census Bureau's shape files see
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2021/TGRSHP2021_TechDoc_Ch3.pdf}{About the 2021 TIGER/Line Shapefiles}.
#'   From \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_Ch4.pdf}{Chapter 4.7 Counties and Equivalent Enties} --
#'   "Counties and equivalent entities are primary legal divisions of states. In most states, these entities are
#'    termed 'counties.'"
#'
#'  A more generalized, recognizable version of the county geometries that has less download size is also available.  For more information on cartographic boundary files see
#'     \href{https://www.census.gov/programs-surveys/geography/technical-documentation/naming-convention/cartographic-boundary-file.html}{Cartographic Boundary File Description}.
#'     These files are available for vintages greater than 2013 with resolution 1:500k, 1:5m, 1:20m meters.
#'
#'  Some earlier vintages may have NA for the crs so you may need to specify the \code{crs_transform} to 3426.  Also
#'    you may be interested in using a state level crs. See \href{https://epsg.io/}{epsg.io} to search worldwide for crs.
#'
#' The function returns the simple feature object which can easily be mapped (see \href{https://github.com/deandevl/RspatialPkg}{RspatialPkg::get_geom_sf()}) or
#' joined with US Census Bureau demographic data via the GEOID value.
#'
#' @param output_dir A full directory path where the shapefile will be downloaded. This is a required parameter. The function will stop 
#' if this directory does not exist.  Be aware that all files in this directory are removed before downloading. 
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#' @param general A logical which if TRUE will download a less detailed, more generalized version of the county geometries.
#' @param resol If \code{general} is TRUE, then the resolution to return. Acceptable values are strings
#'   "500k", "5m", "20m".
#' @param crs_transform A numeric or string that if non-NULL transforms the geometries to this coordinate reference system. See
#'   \href{sf::st_transform()}{https://cran.r-project.org/web/packages/sf/sf.pdf} for acceptable values.
#' @param sf_info A logical which if TRUE displays info on the resulting simple feature object.     
#' @param do_progress A logical which if TRUE displays a progress bar during the download.    
#' @param shapefile A full file path to a shapefile folder with its unzipped files to be processed instead of downloading.
#' @param datafile A dataframe containing data that should be joined with this function's resultant simple feature object.
#' @param datafile_key The column name from \code{datafile} dataframe used to key with the \code{sf_key} column of the resultant simple feature dataframe.
#' @param sf_key The column from the resultant dataframe used to key with the \code{datafile} dataframe.
#' @param express A logical expression object used to filter the resultant simple feature dataframe. 
#'   For example, one of the columns of the resultant simple feature dataframe is "STATEFP".
#'   If you wanted to return just the geometries for Florida (which has a fips code of "12"),
#'   then you assign \code{express} equal to: expression(STATEFP == "12"). The expression will be 
#'   evaluated and only the geometries for Florida will be returned.
#' @param check_na A logical which if TRUE will remove rows that have missing values for any of the columns.
#'   The default is to not check the columns for NA values.   
#'
#' @importFrom sf st_transform
#' @importFrom sf st_as_sf
#' @importFrom sf st_read
#' @importFrom data.table as.data.table
#'
#' @return A data frame object of class sf, data frame
#'
#' @author Rick Dean
#'
#' @export
tiger_counties_sf <- function(
  output_dir = NULL,  
  vintage = 2020,
  general = FALSE,
  resol = "500k",
  crs_transform = NULL,
  sf_info = TRUE,
  do_progress = FALSE,
  shapefile = NULL,
  datafile = NULL,
  datafile_key = NULL,
  sf_key = "COUNTYFP",
  express = NULL,
  check_na = FALSE
){
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
    if(!(resol %in% c("500k", "5m", "20m"))){
      stop("Acceptable values for resolution are '500k', '5m', '20m'.")
    }
  
    if(general){
      if(vintage %in% c(1990, 2000)){
        sub_year <- substr(vintage_char, 3, 4)
        a_url <- sprintf("https://www2.census.gov/geo/tiger/PREVGENZ/co/co%sshp/co99_d%s_shp.zip",
                       sub_year, sub_year)
      }else if(vintage == 2010){
        a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_%s.zip",
                       resol)
      }else {
        if(vintage > 2013){
          a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_county_%s.zip",
                         vintage_char, vintage_char, resol)
        } else {
          a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_us_county_%s.zip",
                         vintage_char, vintage_char, resol)
        }
      }
    }else {
      if(vintage %in% c(2000, 2010)){
        sub_year <- substr(vintage_char, 3, 4)
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/COUNTY/%s/tl_2010_us_county%s.zip",
                         vintage_char, sub_year)
      }else {
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/COUNTY/tl_%s_us_county.zip",
                         vintage_char, vintage_char)
      }
    }
    
    tiger_sf <- .send_tiger_url(
      a_url = a_url, 
      output_dir = output_dir, 
      crs_transform = crs_transform, 
      sf_info = sf_info,
      do_progress = do_progress,
      caller = "tiger_counties_sf")
    
    if(!is.null(datafile)){
      tiger_sf <- RcensusPkg::join_it(
        df_1 = datafile,
        df_2 = tiger_sf,
        key_1 = datafile_key,
        key_2 = sf_key,
        return_sf = T
      )
    }
    
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
