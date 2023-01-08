#' wide_to_long
#'
#' Reshape a data.table from a "wide" format to a "long" format.
#'
#' Function calls data.table::melt() to reshape a wide collection of data.table
#'   columns and their values to a single long column of values.
#'
#' @param dt The data.table with a wide collection of column variables. This parameter is required.
#' @param id_v A vector of column names from \code{dt} that are not to be
#'   consolidated and act as identifier columns for the new long form. This parameter is required.
#' @param measure_v An optional vector of measure column names from \code{dt} that are to be consolidated.
#' @param do_est An optional logical which if TRUE will locate "estimate" columns in \code{dt} and
#'   consolidate their values to the "long" format.
#' @param do_moe An optional logical which if TRUE will locate "margin of error" columns in \code{dt} and
#'   consolidate their values to the "long" format.
#' @param do_est_moe An optional logical which if TRUE will locate both "estimate" and "margin of error" columns
#'   in \code{dt} and consolidate their values to the "long" format.
#' @param variable_name An optional string that sets column name for the consolidated column names.
#' @param value_name An optional string that sets the column name for the consolidated values.
#'
#' @importFrom data.table data.table
#' @importFrom data.table copy
#' @importFrom data.table setnames
#' @importFrom data.table melt
#' @importFrom purrr map
#'
#' @return A reshaped data.table in the "long" format.
#'
#' @author Rick Dean
#'
#' @export
wide_to_long <- function(
  dt = NULL,
  id_v = c("NAME", "GEOID"),
  measure_v = NULL,
  do_est = FALSE,
  do_moe = FALSE,
  do_est_moe = FALSE,
  variable_name = NULL,
  value_name = NULL
){
  get_reshape <- function(df, i_v, meas_v, val_name, var_name){
    sel_var <- c(i_v, meas_v)
    sel_dt <- df[, ..sel_var]
    sel_dt[, id := 1:nrow(sel_dt)]
    long_dt <- stats::reshape(
      sel_dt,
      varying = meas_v,
      v.names = val_name,
      timevar = var_name,
      times = meas_v,
      direction = "long"
    )
    long_sort_dt <- long_dt[order(long_dt$id),]
    long_sort_dt[, id := NULL]
    return(long_sort_dt)
  }
  # Get rid of the last letter in the column name
  new_name_fun <- function(name){
    return(substr(name, 1, nchar(name) - 1))
  }
  get_est <- function(df,i_v){
    dt_copy <- data.table::copy(df)
    col_names_dt <- data.table::data.table(names = names(dt_copy))
    # Get the column names for "estimate" (ends in "E")
    col_names_dt <- col_names_dt[names != "NAME"]
    cols_est <- endsWith(col_names_dt$names, "E")

    # Names of the "estimate" columns in dt_copy
    cols_names_est <- col_names_dt$names[cols_est]

    # Get rid of the "E" letter in the column names
    new_names_est <- unlist(purrr::map(cols_names_est, new_name_fun))

    # Rename the "estimate" columns in dt_copy
    data.table::setnames(dt_copy, old = cols_names_est, new = new_names_est)

    est_melt_dt <- get_reshape(
      df = dt_copy,
      i_v = i_v,
      meas_v = new_names_est,
      val_name = "estimate",
      var_name = "variable"
    )
    return(est_melt_dt)
  }
  get_moe <- function(df,i_v){
    dt_copy <- data.table::copy(df)
    col_names_dt <- data.table::data.table(names = names(dt_copy))
    cols_moe <- endsWith(col_names_dt$names, "M")

    # Names of the "moe" columns in dt_copy
    cols_names_moe <- col_names_dt$names[cols_moe]

    # Get rid of the "M" letter in the "moe" column names
    new_names_moe <- unlist(purrr::map(cols_names_moe, new_name_fun))

    # Rename the "estimate" columns in dt_copy
    data.table::setnames(dt_copy, old = cols_names_moe, new = new_names_moe)

    moe_melt_dt <- get_reshape(
      df = dt_copy,
      i_v = i_v,
      meas_v = new_names_moe,
      val_name = "moe",
      var_name = "variable"
    )
    return(moe_melt_dt)
  }

  if(do_est){
    est_melt_dt <- get_est(dt,id_v)
    return(est_melt_dt)
  }else if(do_moe){
    moe_melt_dt <- get_moe(dt,id_v)
    return(moe_melt_dt)
  }else if(do_est_moe){
    est_melt_dt <- get_est(dt,id_v)
    moe_melt_dt <- get_moe(dt,id_v)
    est_moe_dt <- est_melt_dt[, moe := moe_melt_dt$moe]
    return(est_moe_dt)
  }else if(!is.null(measure_v)){
    long_dt <- get_reshape(
      df = dt,
      i_v = id_v,
      meas_v = measure_v,
      val_name = value_name,
      var_name = variable_name
    )
    return(long_dt)
  }
}
