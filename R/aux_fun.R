#' @export
dt.to.brick <- function(dt, var_name) {
  arr_from_dt <- acast(dt, lat ~ lon ~ time, value.var = var_name, fun.aggregate = mean)
  out <- brick(arr_from_dt, ymn = min(as.numeric(rownames(arr_from_dt))),
               ymx = max(as.numeric(rownames(arr_from_dt))), xmn = min(as.numeric(colnames(arr_from_dt))),
               xmx = max(as.numeric(colnames(arr_from_dt))))
  out <- flip(out, direction = "2")
  return(out)
}

#' @export
csa.rescale <- function(csa_coarse, csa_fine, scale_ratio){
  rescale_factor = csa_fine[scale == scale_ratio]$value
  dummy = csa_coarse
  dummy$scale = dummy$scale * scale_ratio
  dummy$value = t(t(dummy$value) * rescale_factor)
  return(dummy)
}
