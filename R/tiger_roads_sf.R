#' tiger_roads_sf
#'
#' This function performs three tasks:
#' \enumerate{
#'   \item Download to a temporary directory a zip file from the TIGER/Line Shapefiles database.
#'   \item Unzip the zip file and locate the shape file of interest.
#'   \item Read and convert the shape file to a simple feature object.
#' }
#'
#' Returns simple feature (sf) of US Census roads boundary related geometric polygons,
#'   provided by the US Census Bureau's TIGER/Line Shapefiles database. See
#'   \href{https://r-spatial.github.io/sf/articles/sf1.html}{Simple Features for R}
#'   for more information on simple features. Along with the geometries, additional roads related
#'   variables are provided.  See \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_F-R.pdf}{Appendix L-3. Record Layouts: Roads Shapefile)}
#'   for a description of road related variables of the sf file. For further information on the Census Bureau's shape files see
#'   \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2021/TGRSHP2021_TechDoc_Ch3.pdf}{About the 2021 TIGER/Line Shapefiles}.
#'   From \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2022/TGRSHP2022_TechDoc_Ch4.pdf}{Chapter 4.12.3 Roads – Primary, Secondary and All Roads} --
#'   "The primary and secondary roads shapefile contains all linear street features with MTFCCs of primary
#'   roads (S1100) or secondary roads (S1200) in the MAF/TIGER System. Secondary roads are main
#'   arteries, usually in the U.S. highway, state highway, or county highway system. These roads have one or
#'   more lanes of traffic in each direction, may or may not be divided, and usually have at-grade intersections
#'   with many other roads and driveways. These roads often have both a local name and a route number."
#'
#' The function returns the simple feature object which can easily be mapped (see \href{https://github.com/deandevl/RspatialPkg}{RspatialPkg::get_geom_sf()}) or
#' joined with US Census Bureau demographic data via the GEOID value.
#'
#' Some earlier vintages may have NA for the crs so you may need to specify the \code{crs_transform} to 3426.  Also
#'    you may be interested in using a state level crs. See \href{https://epsg.io/}{epsg.io} to search worldwide for crs.
#'
#' @param state A 2-digit FIPS code for the state of interest.
#'   See \href{https://cran.r-project.org/web/packages/usmap/usmap.pdf}{usmap::fips function} for finding FIPS codes.
#' @param county The \emph{three-digit} FIPS code for the county of interest. 
#' @param output_dir A full directory path where the shapefile will be downloaded. This is a required parameter. The function will stop 
#' if this directory does not exist.  Be aware that all files in this directory are removed before downloading. 
#' @param vintage A numeric that sets the vintage of interest. The default is 2020.
#'   The value should be greater than 2010.
#' @param entity A string that defines the category of road geometries to return. Acceptable value are
#'   "us_roads" (state & county arguments not required), "state_roads" (state argument required),
#'    "county_roads" (state & county arguments required).
#' @param crs_transform A numeric or string that if non-NULL transforms the geometries to this coordinate reference system. See
#'   \href{sf::st_transform()}{https://cran.r-project.org/web/packages/sf/sf.pdf} for acceptable values.
#' @param sf_info A logical which if TRUE displays info on the resulting simple feature object.       
#' @param do_progress A logical which if TRUE displays a progress bar during the download.      
#' @param shapefile A full file path to a shapefile folder with its unzipped files to be processed instead of downloading.
#' @param datafile A dataframe containing data that should be joined with this function's resultant simple feature object.
#' @param datafile_key The column name from \code{datafile} used to key with the "tract" column of the resultant simple feature object.
#' @param sf_key The column from the resultant dataframe used to key with the \code{datafile} dataframe.
#' @param express A logical expression object used to filter the resultant simple feature dataframe. 
#'   For example, one of the columns of the resultant simple feature dataframe is "STATEFP".
#'   If you wanted to return just the geometries for Florida (which has a fips code of "12"),
#'   then you assign \code{express} equal to: expression(STATEFP == "12"). The expression will be 
#'   evaluated and only the geometries for Florida will be returned.
#' @param check_na A logical which if TRUE will remove rows that have missing values for any of the columns.
#'   The default is to not check the columns for NA values. 
#'
#'  \strong{Note: vintage must be greater than 2010}
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
tiger_roads_sf <- function(
  state = NULL,
  county = NULL,
  output_dir = NULL,
  vintage = 2020,
  entity = "us_roads",
  crs_transform = NULL,
  sf_info = TRUE,
  do_progress = FALSE,
  shapefile = NULL,
  datafile = NULL,
  datafile_key = NULL,
  sf_key = "GEOID",
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
  
    if(entity == "us_roads"){
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/PRIMARYROADS/tl_%s_us_primaryroads.zip",
                   vintage_char, vintage_char)
    }else if(entity == "state_roads"){
      if(is.null(shapefile) & is.null(state)){
        stop("The state argument is required for the 'state_roads' entity.")
      }
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/PRISECROADS/tl_%s_%s_prisecroads.zip",
                  vintage_char, vintage_char, state)
    }else if(entity == "county_roads"){
      if(is.null(shapefile) & (is.null(state) | is.null(county))){
        stop("The state and county arguments are required for the 'county_roads' entity.")
      }
      a_url <- sprintf("https://www2.census.gov/geo/tiger/TIGER%s/ROADS/tl_%s_%s%s_roads.zip",
                   vintage_char, vintage_char, state, county)
    }else {
      stop(paste0("Entity argument ", entity, " is not recognized."))
    }
  
    tiger_sf <- .send_tiger_url(
      a_url = a_url, 
      output_dir = output_dir, 
      crs_transform = crs_transform, 
      sf_info = sf_info,
      do_progress = do_progress,
      caller = "tiger_roads_sf"
    )
    
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
