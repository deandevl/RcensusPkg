#' tiger_block_groups_sf
#'
#' This function performs three tasks:
#' \enumerate{
#'   \item Download to a temporary directory a zip file from the TIGER/Line Shapefiles database.
#'   \item Unzip the zip file and locate the shape file of interest.
#'   \item Read and convert the shape file to a simple feature object.
#' }
#'
#' Returns simple feature (sf) of US Census block group boundary related geometric polygons,
#'   provided by the US Census Bureau's TIGER/Line Shapefiles database. See
#'   \href{https://r-spatial.github.io/sf/articles/sf1.html}{Simple Features for R}
#'   for more information on simple features. Along with the geometries, additional block group related
#'   variables are provided.  See \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_F-R.pdf}{Appendix G-2. Record Layout: Block Group State-based Shapefile)}
#'   for a description of block group related variables of the sf file. For further information on the Census Bureau's shape files see
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2021/TGRSHP2021_TechDoc_Ch3.pdf}{About the 2021 TIGER/Line Shapefiles}.
#'   From \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_Ch4.pdf}{Chapter 4.3 Block Groups} --
#'   "Standard block groups are clusters of blocks within the same census tract that have the same first digit of
#'   their 4-character census block number (e.g., Blocks 3001, 3002, 3003 to 3999 in census tract 1210.02
#'   belong to block group 3). Current block groups do not always maintain these same block number to block
#'   group relationships due to boundary and feature changes that occur throughout the decade. A block group usually covers a contiguous area. Each census tract contains one or more block groups
#'   and block groups have unique numbers within census tract."
#'
#'   A more generalized, recognizable version of the block group geometries that has less download size is also available.  For more information on cartographic boundary files see
#'     \href{https://www.census.gov/programs-surveys/geography/technical-documentation/naming-convention/cartographic-boundary-file.html}{Cartographic Boundary File Description}.
#'     These files are available for vintages greater than 2013 with resolution 1:500k meters.
#'
#'   Some earlier vintages may have NA for the crs so you may need to specify the \code{crs_transform} to 3426.  Also
#'    you may be interested in using a state level crs. See \href{https://epsg.io/}{epsg.io} to search worldwide for crs.
#'
#' The function returns the simple feature object which can easily be mapped (see \href{https://github.com/deandevl/RspatialPkg}{RspatialPkg::get_geom_sf()}) or
#' joined with US Census Bureau demographic data via the GEOID value.
#'
#' @param state A two-digit FIPS code for the state of interest. This is a required parameter.
#'   See \href{https://cran.r-project.org/web/packages/usmap/usmap.pdf}{usmap::fips function} for finding FIPS codes.
#' @param output_dir A full directory path where the shapefile will be downloaded. This is a required parameter. The function will stop 
#' if this directory does not exist.  Be aware that all files in this directory are removed before downloading. 
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#' @param general A logical which if TRUE will download a less detailed, more gerneralized version of the block group geometries.
#' @param sf_info A logical which if TRUE displays info on the resulting simple feature object.    
#' @param do_progress A logical which if TRUE displays a progress bar during the download.    
#' @param crs_transform A numeric or string that if non-NULL transforms the geometries to this coordinate reference system. See
#'   \href{sf::st_transform()}{https://cran.r-project.org/web/packages/sf/sf.pdf} for acceptable values.
#' @param shapefile A full file path to a shapefile folder with its unzipped files to be processed instead of downloading.
#' @param express A logical expression object used to filter the resultant simple feature dataframe. 
#'   For example, one of the columns of the resultant simple feature dataframe is "COUNTYFP".
#'   If you wanted to return just the geometries for Los Alamos, New Mexico (which has a fips code of "028"),
#'   then you assign \code{express} equal to: expression(COUNTYFP == "028"). The expression will be 
#'   evaluated and only the tract geometries for Los Alamos will be returned.
#' @param check_na A logical which if TRUE will remove rows that have missing values for any of the columns.
#'   The default is to not check the columns for NA values. 
#'
#' @importFrom sf st_as_sf
#' @importFrom sf st_read
#' @importFrom data.table as.data.table
#'
#' @return A data frame object of class sf, data frame
#'
#' @author Rick Dean
#'
#' @export
tiger_block_groups_sf <- function(
  state = NULL,
  output_dir = NULL,
  vintage = 2020,
  general = FALSE,
  sf_info = TRUE,
  do_progress = FALSE,
  crs_transform = NULL,
  shapefile = NULL,
  express = NULL,
  check_na = FALSE
){
  if(is.null(shapefile) & is.null(state)){
    stop("The state argument is required")
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
  }else {  # Downloading shapefile
    vintage_char <- as.character(vintage)
    a_url <- NULL
    if(general){
      if(vintage %in% c(1990, 2000)){
        sub_year <- substr(vintage_char, 3, 4)
        a_url <- sprintf("https://www2.census.gov/geo/tiger/PREVGENZ/bg/bg%sshp/bg%s_d%s_shp.zip",
                       sub_year, state, sub_year)
      }else if(vintage == 2010){
        a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ2010/gz_2010_%s_150_00_500k.zip",
                       state)
      }else{
        if(vintage > 2013){
          a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_%s_bg_500k.zip",
                         vintage_char, vintage_char, state)
        }else {
          a_url <- sprintf("https://www2.census.gov/geo/tiger/GENZ%s/cb_%s_%s_bg_500k.zip",
                         vintage_char, vintage_char, state)
        }
      }
    }else {
      if(vintage %in% c(2000, 2010)){
        sub_vintage <- substr(vintage_char, 3, 4)
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER2010/BG/%s/tl_2010_%s_bg%s.zip",
                         vintage_char, state, sub_vintage)
      }else {
        a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/BG/tl_%s_%s_bg.zip",
                         vintage_char, vintage_char, state)
      }
    }
    tiger_sf <- .send_tiger_url(
      a_url = a_url, 
      output_dir = output_dir, 
      crs_transform = crs_transform, 
      sf_info = sf_info,
      do_progress = do_progress,  
      caller = "tiger_block_groups_sf"
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
