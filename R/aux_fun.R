#' Trasnform data.table to brick
#'
#' The function \code{dt.to.brick} tranforms a data.table object to brick (raster) format
#'
#' @param dt The data table object to be transformed. It must be in a four-column format, with the coordinate columns named as "lat" & "lon"
#' and time values as "time".
#' @param var_name The name (chr) of the column in the data table (\code{dt}) wich holds the values of the variable, e.g., "temperature".
#' @return \code{dt} as a brick object.
#' @export
#' @examples
#'
#'aa <- expand.grid(lat = seq(40, 50, 1),
#'                  lon = seq(20, 30, 1),
#'                  time = seq(1900, 2000, 1))
#'aa$anomaly = rnorm(nrow(aa))
#'aa <- brick(dt.to.brick(aa, "anomaly"))

dt.to.brick <- function(dt, var_name) {
  arr_from_dt <- acast(dt, lat ~ lon ~ time, value.var = var_name, fun.aggregate = mean)
  out <- brick(arr_from_dt, ymn = min(as.numeric(rownames(arr_from_dt))),
               ymx = max(as.numeric(rownames(arr_from_dt))), xmn = min(as.numeric(colnames(arr_from_dt))),
               xmx = max(as.numeric(colnames(arr_from_dt))))
  out <- flip(out, direction = "2")
  return(out)
}

#' Changes the scale of an aggregation curve
#'
#' The function \code{csa.rescale} needs to be desribed.
#'
#' @param csa_coarse
#' @param csa_fine
#' @param scale_ratio
#' @return as a brick object.
#' @export
#' @examples

csa.rescale <- function(csa_coarse, csa_fine, scale_ratio){
  rescale_factor = csa_fine[scale == scale_ratio]$value
  dummy = csa_coarse
  dummy$scale = dummy$scale * scale_ratio
  dummy$value = t(t(dummy$value) * rescale_factor)
  return(dummy)
}

aa <- expand.grid(lat = seq(40, 50, 1),
                      lon = seq(20, 30, 1),
                      time = seq(1900, 2000, 1))
aa$anomaly = rnorm(nrow(aa))
aa <- brick(dt.to.brick(aa, "anomaly"))
