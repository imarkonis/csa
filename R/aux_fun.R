#' Transform data.table to brick
#'
#' The function \code{dt.to.brick} transforms a data.table object to brick (raster) format
#'
#' @param dt The data table object to be transformed. It must be in a four-column format, with the coordinate columns named as "lat" & "lon"
#' and time values as "time".
#' @param var_name The name (chr) of the column in the data table (\code{dt}) which holds the values of the variable, e.g., "temperature".
#' @return \code{dt} as a brick object.
#' @export
#' @examples
#'\dontrun{
#'aa <- expand.grid(lat = seq(40, 50, 1),
#'                  lon = seq(20, 30, 1),
#'                  time = seq(1900, 2000, 1))
#'aa$anomaly = rnorm(nrow(aa))
#'aa <- brick(dt.to.brick(aa, "anomaly"))
#'}


dt.to.brick <- function(dt, var_name) {
  arr_from_dt <- acast(dt, lat ~ lon ~ time, value.var = var_name, fun.aggregate = mean)
  out <- brick(arr_from_dt, ymn = min(as.numeric(rownames(arr_from_dt))),
               ymx = max(as.numeric(rownames(arr_from_dt))), xmn = min(as.numeric(colnames(arr_from_dt))),
               xmx = max(as.numeric(colnames(arr_from_dt))))
  out <- flip(out, direction = "2")
  return(out)
}

