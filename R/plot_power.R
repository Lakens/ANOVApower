#' Convenience function to plot power across a range of sample sizes.
#' @param design_result Output from the ANOVA_design function
#' @param min_n Minimum sample size in power curve.
#' @param max_n Maximum sample size in power curve.
#' @param plot Should power plot be printed (defaults to TRUE)
#' @return Returns plot with power curves for the ANOVA, and a dataframe with the summary data.
#' @examples
#' design_result <- ANOVA_design(design = "3b",
#'                              n = 20,
#'                              mu = c(0,0,0.3),
#'                              sd = 1,
#'                              labelnames = c("condition",
#'                              "cheerful", "neutral", "sad"))
#'
#' plot_power(design_result, max_n = 300)
#' @section References:
#' too be added
#' @import ggplot2
#' @export

plot_power <- function(design_result, min_n = 7, max_n = 100, plot = TRUE){
  design = design_result$design
  mu = design_result$mu
  sd <- design_result$sd
  r <- design_result$r
  labelnames <- design_result$labelnames

  #Do one ANOVA to get number of power columns
  length_power <- length(ANOVA_exact(design_result, verbose = FALSE)$main_results$power)

  power_df <- as.data.frame(matrix(0, ncol = length_power+1, nrow = max_n+1-min_n))
  power_df[,1] <- c((min_n):max_n)

  colnames(power_df) <- c("n", row.names(ANOVA_exact(design_result, verbose = FALSE)$main_results))

  for (i in 1:(max_n+1-min_n)){
    design_result <- ANOVA_design(design = design,
                                  n = i+min_n,
                                  mu = mu,
                                  sd = sd,
                                  r = r,
                                  labelnames = labelnames,
                                  plot = FALSE)

    power_df[i,2:(1+length_power)] <- ANOVA_exact(design_result, verbose = FALSE)$main_results$power
  }
  plot_data <- suppressMessages(melt(power_df, id = c('n')))

  p1 <- ggplot(data=plot_data, aes(x = n, y = value)) +
    geom_line( size=1.5) +
    scale_x_continuous(limits = c(min_n, max_n)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,10)) +
    theme_bw() +
    labs(x="Sample size per condition", y = "Power") +
    facet_grid(variable ~ .)

  if(plot == TRUE){
    print(p1)
  }

  invisible(list(p1 = p1,
                 power_df = power_df))
}
