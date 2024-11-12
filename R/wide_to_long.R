#' wide_to_long
#'
#' Reshape a data frame from a "wide" format to a "long" format.
#'
#' Function is a helper in calling data.table's melt() function
#'   to reshape a wide data frame to a long form.
#'
#' @param dt The data frame with a wide collection of column variables.
#'   This parameter is required.
#' @param id_v A character vector of column from \code{dt} that are not to be
#'   consolidated and act as identifier columns for the new long form.
#'   This parameter is required.
#' @param measure_v An optional character vector that sets the column measures from
#'   \code{dt} that are to be consolidated. If not specified then all the columns
#'   not in \code{id_v} are considered measures and will be consolidated.
#' @param variable_name An optional string that sets the column name for
#'   the consolidated column names.
#' @param value_name An optional string that sets the column name for the
#'   consolidated values.
#' @param na_rm An optional logical which if TRUE will remove rows with NA values.
#'
#' @importFrom data.table as.data.table melt copy
#'
#' @return A reshaped data frame in the "long" format.
#'
#' @author Rick Dean
#'
#' @export
wide_to_long <- function(
    dt = NULL,
    id_v = c("NAME","GEOID"),
    measure_v = NULL,
    variable_name = "variable",
    value_name = "estimate",
    na_rm = FALSE
){
  dt_copy <- data.table::copy(dt)
  dt_copy_dt <- data.table::as.data.table(dt_copy)

  return_dt <- data.table::melt(
    data = dt_copy_dt,
    id.vars = id_v,
    measure.vars = measure_v,
    variable.name = variable_name,
    value.name = value_name,
    na.rm = na_rm
  )
  return(return_dt)
}
