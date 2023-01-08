#' long_to_wide
#' 
#' Reshape a data.table from a "long" format to a "wide" format.
#' 
#' Function calls data.table::dcast() to reshape a long single column and
#'   its values to multilpe colums.
#'   
#' @param dt The data.table with a long column format. This parameter is required.
#' @param id_v A vector of column names from \code{dt} that act as identifiers and
#'   are not part of the widened column. This parameter is required.
#' @param parameter_col A column name from \code{dt} whose unique values will become 
#'   column names for the new expanded data.table.
#' @param value_col A column name or vector of column names from \code{dt} whose values will fall under
#'   the new expanded data.table. This parameter is required.
#' 
#' @importFrom data.table copy
#' @importFrom data.table dcast
#' @importFrom data.table setnames
#'
#' @return A reshaped data.table in the "wide" format.
#' 
#' @author Rick Dean
#' 
#' @export
long_to_wide <- function(
  dt = NULL,
  id_v = c("NAME", "GEOID"),
  parameter_col = NULL,
  value_col = NULL
){
  # Check arguments
  if(is.null(dt)){
    stop("The 'dt' argument is required.")
  }
  if(is.null(parameter_col)){
    stop("The 'parameter_col' argument is required.")
  }
  if(is.null(value_col)){
    stop("The 'value_col' argument is required.")
  }
  
  dt_copy <- data.table::copy(dt)
  #col_names_dt <- data.table::data.table(names = names(dt_copy))
  
  # Create the formula for dcast
  a_formula_str <- paste(id_v, collapse = "+")
  a_formula_str <- paste0(a_formula_str, " ~ ", parameter_col)
  
  dt_wide_dt <- data.table::dcast(
    data = dt,
    formula = as.formula(a_formula_str),
    value.var = value_col
  )
  
  if("estimate" %in% value_col & "moe" %in% value_col){
    for(name in names(dt_wide_dt)){
      if(startsWith(name, "estimate")){
        data.table::setnames(dt_wide_dt, old = name, new = paste0(substr(name, start = 10, stop = nchar(name)), "E"))
      }else if(startsWith(name, "moe")){
        data.table::setnames(dt_wide_dt, old = name, new = paste0(substr(name, start = 5, stop = nchar(name)), "M"))
      }
    }
  }
  
  return(dt_wide_dt)
}