#' Estimate and print the temporal CSA plot
#'
#' The function \code{csa} computes (and by default plots) the aggregation curve of a given statistic in a single dimension, e.g., time.
#'
#' @param x A numeric vector.
#' @param stat The statistic which will be estimated across the cross-scale continuum. Suitable options are:
#' \itemize{
#' \item{"var" for variance,}
#' \item{"sd" for standard deviation,}
#' \item{"skew" for skewness,}
#' \item{"kurt" for kurtosis,}
#' \item{"l2" for L-scale,}
#' \item{"t2" for coefficient of L-variation,}
#' \item{"t3" for L-skewness,}
#' \item{"t4" for L-kurtosis.}
#' }
#' @param std logical. If TRUE (the default) the CSA plot is standardized to unit, i.e., zero mean and unit variance in the original time scale.
#' @param threshold numeric. Sample size of the time series at the last aggregated scale.
#' @param plot logical. If TRUE (the default) the CSA plot is printed.
#' @param fast logical. If TRUE the CSA plot is estimated only in logarithmic scale; 1, 2, 3, ... , 10, 20, 30, ... , 100, 200, 300 etc.
#' @param ... log_x and log_y (default TRUE) for setting the axes of the CSA plot to logarithmic scale. The argument wn (default FALSE) is used to plot a line presenting the standardized variance of the white noise process. Therefore, it should be used only with stat = "var" and std = T.
#' @return
#' If \code{plot = TRUE}, the \code{csa} returns a list containing:
#'  \itemize{
#'  \item{\code{values}: Matrix of the timeseries values for the selected \code{stat} at each \code{scale}.}
#'  \item{\code{plot}: Plot of \code{scale} versus \code{stat} as a \emph{ggplot} object.}
#'  }
#'  If \code{plot = FALSE}, then it returns only the matrix of the timeseries values for the selected \code{stat} at each \code{scale}.
#' @export
#' @examples
#' csa(rnorm(1000), wn = TRUE)
#' data(gpm_nl, knmi_nl, rdr_nl, ncep_nl, cnrm_nl, gpm_events)
#' csa(knmi_nl$prcp, threshold = 10, fast = TRUE)
#'
#' csa(gpm_nl$prcp, stat = "skew", std = FALSE, log_x = FALSE, log_y = FALSE, smooth = TRUE)
#'
#' gpm_skew <- csa(gpm_nl$prcp, stat = "skew", std = FALSE, log_x = FALSE, log_y = FALSE,
#' smooth = TRUE, plot = FALSE)
#' rdr_skew <- csa(rdr_nl$prcp, stat = "skew", std = FALSE, log_x = FALSE, log_y = FALSE,
#' smooth = TRUE, plot = FALSE)
#' csa.multiplot(rbind(data.frame(gpm_skew, dataset = "gpm"), data.frame(rdr_skew,
#' dataset = "rdr")), log_x = FALSE, log_y = FALSE, smooth = TRUE)
#'
#' set_1 <- data.frame(csa(gpm_nl$prcp, plot = FALSE, fast = TRUE), dataset = "gpm")
#' set_2 <- data.frame(csa(rdr_nl$prcp, plot = FALSE, fast = TRUE), dataset = "radar")
#' set_3 <- data.frame(csa(knmi_nl$prcp, plot = FALSE, fast = TRUE), dataset = "station")
#' set_4 <- data.frame(csa(ncep_nl$prcp, plot = FALSE, fast = TRUE), dataset = "ncep")
#' set_5 <- data.frame(csa(cnrm_nl$prcp, plot = FALSE, fast = TRUE), dataset = "cnrm")
#' csa.multiplot(rbind(set_1, set_2, set_3, set_4, set_5))
#' @references Markonis et al., A cross-scale analysis framework for model/data comparison and integration, Geoscientific Model Development, Submitted.

csa  <- function(x, stat = "var", std = TRUE, threshold = 30, plot = TRUE, fast = FALSE, ...) {
  if (!is.numeric(x)) stop ("x should be numeric.")
  if (!is.vector(x)) stop ("x should be vector.")
  '%!in%' <- function(x, y)!('%in%'(x, y)) # keep function inside for the 'parallel' package
  if (stat %!in% c("sd", "var", "skew", "kurt", "cv", "l2", "t2", "t3", "t4"))
    stop("Error: Invalid stat. Select one of sd, var, skew, kurt, cv, l2, t2, t3, t4.")

  out <- vector()
  nna <- sum(!is.na(x)) # actual length without accounting for missing values
  max_agg_scale <- round(nna / threshold, 0) # aggregation scale up to sample size of 30 values does not count NAs
  timescales <- 1:max_agg_scale
  if(fast == TRUE){
    timescales <- c(1:9 %o% 10^(0:30))
    timescales <- timescales[timescales <= max_agg_scale]
  }
  # Parallel computing
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster = makeCluster(no_cores, type = "SOCK")
  registerDoSNOW(cluster)

  if (max_agg_scale != 0 & nna > 2 * threshold){ # check for adequate time series length
    if (std == TRUE){  # standardize
      x <- scale(x, center = TRUE, scale = TRUE)
    }
    out <- foreach (i = timescales, .combine = 'c') %dopar%  {# parallel loop around aggregation scale
      x_agg <- as.numeric(tapply(x, (seq_along(x) - 1) %/% i, mean, na.rm = TRUE)) # estimate aggregated values for scale i
      if (sum(!is.na(x_agg)) > threshold){ # have at least 30 values for the y_scale estimation
        # classic moments ------------------------------------------------------
        if (stat == "sd"){sd(x_agg, na.rm = TRUE)}
        else if (stat == "var"){var(x_agg, na.rm = TRUE)}
        else if (stat == "skew"){skewness(x_agg, na.rm = TRUE)}
        else if (stat == "kurt"){kurtosis(x_agg, na.rm = TRUE)}
        # L-moments ------------------------------------------------------------
        else if (stat == "l2"){Lmoments(x_agg, rmax = 2, na.rm = TRUE)[, "L2"]}  # stat: L2, L-scale
        else if (stat == "t2"){out[i, "y_scale"] <- Lmoments(x_agg, rmax = 2, na.rm = TRUE)[, "L2"] /
          Lmoments(x_agg, rmax = 2, na.rm = TRUE)[, "L1"]}                       # stat: L-moment ratio L2/L1
        else if (stat == "t3"){Lcoefs(x_agg,rmax = 4, na.rm = TRUE)[, "tau3"]}   # stat: L-moment ratio L3/L2
        else if (stat == "t4"){Lcoefs(x_agg, rmax = 4, na.rm = TRUE)[, "tau4"]}  # stat: L-moment ratio L4/L3
      }
    } # parallel loop around aggregation scale
  } else {
    return("Error: Time series length too short!")
  }
  stopCluster(cluster)
  out <- out[-length(out)]
  out <- matrix(c(timescales[1:length(out)], out), ncol = 2)
  colnames(out) = c("scale", stat)

  if (plot == TRUE){
    plot_sc <- csa.plot(out, ...)
    return(list(values = out,
                plot = plot_sc))
  }
  else {
    return(out)
  }
}

#' Estimate and print the spatial CSA plot
#'
#' The function \code{csa} computes (and by default plots) the aggregation curve of a given statistic in two dimensions, e.g., space.
#'
#' @param x A raster or brick object.
#' @param stat The statistic which will be estimated across the cross-scale continuum. Suitable options are:
#' \itemize{
#' \item{"var" for variance,}
#' \item{"sd" for standard deviation,}
#' \item{"skew" for skewness,}
#' \item{"kurt" for kurtosis,}
#' \item{"l2" for L-scale,}
#' \item{"t2" for coefficient of L-variation,}
#' \item{"t3" for L-skewness,}
#' \item{"t4" for L-kurtosis.}
#' }
#' @param std logical. If TRUE (the default) the CSA plot is standardized to unit, i.e., zero mean and unit variance in the original time scale.
#' @param threshold numeric. Sample size of the time series at the last aggregated scale.
#' @param plot logical. If TRUE (the default) the CSA plot is printed
#' @param fast logical. If TRUE the CSA plot is estimated only in logarithmic scale; 1, 2, 3, ... , 10, 20, 30, ... , 100, 200, 300 etc.
#' @param ... log_x and log_y (default TRUE) for setting the axes of the CSA  plot to logarithmic scale. The argument wn (default FALSE) is used to plot a line presenting the standardized variance of the white noise process. Therefore, it should be used only with stat = "var" and std = T.
#'
#' @return
#' If \code{plot = TRUE}, the \code{csa} returns a list containing:
#'  \itemize{
#'  \item{\code{values}: Matrix of the timeseries values for the selected \code{stat} at each \code{scale}.}
#'  \item{\code{plot}: Plot of \code{scale} versus \code{stat} as a \emph{ggplot} object.}
#'  }
#'  If \code{plot = FALSE}, then it returns only the matrix of the timeseries values for the selected \code{stat} at each \code{scale}.
#'
#' @export
#' @examples
#' data(gpm_events)
#' event_dates <- format(gpm_events[, unique(time)], "%d-%m-%Y")
#' gpm_events_brick <- dt.to.brick(gpm_events, var_name = "prcp")
#' plot(gpm_events_brick, col = rev(colorspace::sequential_hcl(40)),
#'      main = event_dates)
#' csas(gpm_events_brick)
#'
#' gpm_sp_scale <- csas(gpm_events_brick, plot = FALSE)
#' gpm_sp_scale[, variable := factor(variable, labels = event_dates)]
#' csa.multiplot(gpm_sp_scale, smooth = TRUE, log_x = FALSE, log_y = FALSE)
#'
#' @references Markonis et al., A cross-scale analysis framework for model/data comparison and integration, Geoscientific Model Development, Submitted.

csas <- function(x, stat = "var", std = TRUE, plot = TRUE, threshold = 30, ...){
  '%!in%' <- function(x, y)!('%in%'(x, y)) # keep function inside for the 'parallel' package
  if (stat %!in% c("sd", "var", "skew", "kurt", "cv", "l2", "t2", "t3", "t4"))
    stop("Error: Invalid stat. Select one of sd, var, skew, kurt, cv, l2, t2, t3, t4.")

  ncells <- x@ncols * x@nrows
  max_agg_scale <- round(sqrt(ncells / threshold), 0) # aggregation scale up to sample size of 30 values does not count NAs
  x_agg <- list()

  # Parallel computing
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster = makeCluster(no_cores, type = "SOCK")
  registerDoSNOW(cluster)

  if (max_agg_scale != 0 & ncells > 2 * threshold){ # check for adequate time series length
    no_layer <- nlayers(x)

    out <- foreach (j = 1:no_layer, .packages = "raster") %dopar%  {# parallel loop around aggregation scale
      if(no_layer > 1){
        x_layer <- raster(x, layer = j)
      }
      else{
        x_layer <- x
      }
      if (std == TRUE){  # standardize
        x_layer[,] <- scale(x_layer[,], center = TRUE, scale = TRUE)
      }
      for (i in 1:max_agg_scale){
        x_agg[[i]] <- aggregate(x_layer, na.rm = TRUE, fact = i)
      }
      if (stat == "sd"){sapply(sapply(x_agg, getValues), sd, na.rm = TRUE)}
      else if (stat == "var"){sapply(sapply(x_agg, getValues), var, na.rm = TRUE)}
      else if (stat == "skew"){sapply(sapply(x_agg, getValues), skewness, na.rm = TRUE)}
      else if (stat == "kurt"){sapply(sapply(x_agg, getValues), mean, na.rm = TRUE)}
    } # parallel loop around aggregation scale

    out <- data.table(melt(out))
    out$scale <- rep(1:nrow(out[L1 == 1]), max(out$L1))
    colnames(out)[2] = "variable"
    out <- out[, c(3, 1, 2)]
  } else {
    return("Error: Time series length too short!")
  }
  stopCluster(cluster)

  if (plot == TRUE){
    if(ncol(out) == 2){
      plot_sc <- csa.plot(out, ...)
    }
    else{
      plot_sc <- csa.multiplot(out, ...)
    }
    return(list(values = out,
                plot = plot_sc))
  }
  else {
    return(out)
  }
}
