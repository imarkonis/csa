#' CSA curve plotting
#'
#' Function for plotting single CSA curves.
#'
#' @param x A matrix or data.frame composed of two columns;
#' scale for the temporal or spatial scale and value for the estimate of a given statistic (e.g., variance) at the given aggregated scale.
#' @param log_x logical. If TRUE (the default) the x axis of the CSA plot is set to the logarithmic scale.
#' @param log_y logical. If TRUE (the default) the y axis of the CSA plot is set to the logarithmic scale.
#' @param smooth logical. If TRUE (the default) the aggregation curves are smoothed (loess function).
#' @param wn logical. The argument wn (default FALSE) is used to plot a line presenting the standardized variance of the white noise process.
#' Therefore, it should be used only with stat = "var" and std = T in the csa/csas functions.
#' @return The CSA plot as a ggplot object.
#' @export
#' @examples
#' \dontrun{
#' aa <- rnorm(1000)
#' csa_aa <- csa(aa, plot = FALSE)
#' csa.plot(csa_aa)
#' }

csa.plot <- function(x, log_x = TRUE, log_y = TRUE, smooth = FALSE, wn = FALSE){
  colnames(x) <- c("scale", "Value")
  df <- as.data.frame(x)
  if(smooth == FALSE){
    gp <- ggplot(data = df, aes_string(x = "scale", y = "Value")) +
      geom_line(size = 0.5) +
      geom_point() +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())}
  else{
    gp <- ggplot(data = df, aes_string(x = "scale", y = "Value")) +
      geom_line(size = 1,
                stat = 'smooth', method = "loess", se = FALSE, span = 1) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())}
  if(log_x == TRUE){
    gpp <- gp + scale_x_log10("Aggregation scale [-]",
                              labels = trans_format("log10", math_format(10 ^ .x)),
                              breaks = trans_breaks("log10",
                                                    n = abs(round(log10(min(df[, 2], na.rm = TRUE)))) + 1,
                                                    function(x) 10 ^ x)) +
      annotation_logticks(sides = "b")
  } else {
    gpp <- gp + scale_x_continuous("Aggregation scale [-]")
  }
  if(log_y == TRUE){
    gppp <- gpp + scale_y_log10("Value", labels = trans_format("log10", math_format(10^.x)),
                                breaks = trans_breaks("log10",
                                                      n = abs(round(log10(min(df[, 2], na.rm = TRUE)))) + 1,
                                                      function(x) 10 ^ x)) +
      annotation_logticks(sides = "b")
  } else {
    gppp <- gpp + scale_y_continuous("Value")
  }
  if(wn == TRUE){
    gppp <- gppp + geom_abline(slope = -1, size = 1, col = "dark gray", linetype = "dashed")
  }
  gppp
}

#' Multiple CSA plotting
#'
#' Function for plotting multiple CSA curves in a single plot.
#'
#' @param df A matrix or data.frame composed of three columns;
#' scale for the temporal or spatial scale; value for the estimate of a given statistic (e.g., variance) at the given aggregated scale and
#' variable for defining the corresponding dataset.
#' @param log_x logical. If TRUE (the default) the x axis of the CSA plot is set to the logarithmic scale.
#' @param log_y logical. If TRUE (the default) the y axis of the CSA plot is set to the logarithmic scale.
#' @param smooth logical. If TRUE (the default) the aggregation curves are smoothed (loess function).
#' @param wn logical. The argument wn (default FALSE) is used to plot a line presenting the standardized variance of the white noise process.
#' Therefore, it should be used only with stat = "var" and std = T in the csa/csas functions.
#' @return The CSA plot as a ggplot object.
#' @export
#' @examples
#' \dontrun{
#' aa <- rnorm(1000)
#' csa_aa <- data.frame(csa(aa, plot = FALSE), variable = 'wn')
#' bb <- as.numeric(arima.sim(n = 1000, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488))))
#' csa_bb <- data.frame(csa(bb, plot = FALSE), variable = 'arma(2, 2)')
#' csa.multiplot(rbind(csa_aa, csa_bb), wn = TRUE)
#' csa.multiplot(rbind(csa_aa, csa_bb), wn = TRUE, smooth = TRUE)
#' }

csa.multiplot <- function(df, log_x = TRUE, log_y = TRUE, wn = FALSE, smooth = FALSE){
  colnames(df) <- c("scale", "value", "variable")
  df <- as.data.frame(df)
  no_var <- length(unique(df$variable))
  cols <- colorRampPalette(c("#4575b4", "#abd9e9"), space = "rgb")(no_var)
  transp = 1 / log(no_var)

  if(no_var <= 10){
    transp = 1
    cols <- c("#807dba", "#74add1", "#78c679", "#fdae61", "#f46d43",
              "#d73027", "#fee090", "#d9f0a3", "#abd9e9", "#4575b4")[1:no_var]
  }
  if(smooth == FALSE){
    gp <- ggplot(data = df, aes_string(x = df[ ,1], y = df[ ,2])) +
      geom_line(aes(group = interaction(variable),
                    colour = factor(variable)), size = 0.5, alpha = transp) +
      geom_point(aes(group = interaction(variable),
                     colour = factor(variable)), alpha = transp) +
      scale_colour_manual("", values = cols) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())
  }
  else{
    gp <- ggplot(data = df, aes_string(x = df[ ,1], y = df[ ,2])) +
      geom_line(aes(group = interaction(variable),
                    colour = factor(variable)),
                stat='smooth', method = "loess", se = FALSE, span = 1,
                size = 1, alpha = transp) +
      scale_colour_manual("", values = cols) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank())
  }
  if(no_var > 10){gp <- gp + rremove("legend")}
  if(log_x == TRUE){
    gpp <- gp + scale_x_log10("Aggregation scale [-]",
                              labels = trans_format("log10", math_format(10 ^ .x)),
                              breaks = trans_breaks("log10",
                                                    n = abs(round(log10(min(df[, 2], na.rm = TRUE)))) + 1,
                                                    function(x) 10 ^ x)) +
      annotation_logticks(sides = "b")
  } else {
    gpp <- gp + scale_x_continuous("Aggregation scale [-]")
  }
  if(log_y == TRUE){
    gppp <- gpp + scale_y_log10("Value", labels = trans_format("log10", math_format(10^.x)),
                                breaks = trans_breaks("log10",
                                                      n = abs(round(log10(min(df[, 2], na.rm = TRUE)))) + 1,
                                                      function(x) 10 ^ x)) +
      annotation_logticks(sides = "b")
  } else {
    gppp <- gpp + scale_y_continuous("Value")
  }
  if(wn == TRUE){
    gppp <- gppp + geom_abline(slope = -1, size = 1, col = "dark gray", linetype = "dashed")
  }
  gppp
}
