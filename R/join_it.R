#' join_it
#' 
#' Outer join two dataframes that have a common column variable.
#' 
#' Function uses fast data.table techniques to join two data.tables
#'   by their common key values.  Examples might include using the "GEOID" variable 
#'   as a key to join data from RcensusPkg::get_vintage_data() with a 
#'   simple feature with its geometries for counties, states, countries for example
#'   from RcensusPkg::tiger_*_sf(). The resulting dataframe could then display the
#'   geometries (with RspatialPkg::get_geom_sf()) with an aesthetic mapping  
#'   (e.g. fill/color/size) with a joined data column. Joining could also take place
#'   between two simple features (created by RcensusPkg::tiger_*_sf()) or between 
#'   two dataframes created by (created by RcensusPkg::get_vintage_data()).
#'   
#'   The important thing to remember is that all the rows in \code{df_2} will
#'   be present in the resultant data.table.
#'   
#' @param df_1 The first dataframe to be joined. 
#' @param df_2 The second dataframe to be joined with \code{df_1}. All rows in
#'   this dataframe will be present in the resultant dataframe.
#' @param key_1 A string that names the column from \code{df_1} that is common to \code{df_2}.
#' @param key_2 A string that names the column from \code{df_2} that is common to \code{df_1}.
#' @param negate An optional logical which if TRUE will return a dataframe
#'   that has rows in \code{df_1} but not in \code{df_2}.
#' @param match An optional logical which if TRUE will return a dataframe
#'   that has rows where only both \code{df_1} and \code{df_2} have matches.
#' @param return_sf An optional logical which if TRUE will convert the resultant
#'   data.table to a simple feature if it has a geometries column.
#' @param na_rm An optional logical which if TRUE (the default) then remove rows
#'   with NA values.
#'   
#' @importFrom data.table as.data.table
#' @importFrom data.table setkeyv
#' @importFrom sf st_as_sf
#' 
#' @return A data.table or simple feature object if \code{return_sf} is TRUE.
#' 
#' @author Rick Dean
#' 
#' @export
join_it <- function(
  df_1 = NULL,
  df_2 = NULL,
  key_1 = NULL,
  key_2 = NULL,
  negate = FALSE,
  match = FALSE,
  return_sf = FALSE,
  na_rm = TRUE
){
  dt_1 <- data.table::as.data.table(df_1, na.rm = na_rm)
  dt_2 <- data.table::as.data.table(df_2, na.rm = na_rm)
  
  # set the keys
  data.table::setkeyv(dt_1, key_1)
  data.table::setkeyv(dt_2, key_2)
  
  result_dt <- NULL
  
  if(negate){
    result_dt <- dt_1[!dt_2]
  }else if(match){
    result_dt <- dt_1[dt_2, nomatch = 0]
  }else {
    result_dt <- dt_1[dt_2]
  }
  
  if(return_sf){
    result_dt <- sf::st_as_sf(result_dt)
  }
  
  return(result_dt)
}
   