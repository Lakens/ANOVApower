design_result <- ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 1, 0),
                              sd = 2, r = 0.8)
max_n <- 50
#' Convenience function to plot power across a range of sample sizes.
#' @param design_result Output from the ANOVA_design function
#' @param max_n Maximum sample size in power curve.
#' @return Returns plots with power curves for the effects, and a dataframe with the summary data.
#' @examples
#' design_result <- ANOVA_design(design = "3b",
#'                              n = 20,
#'                              mu = c(0,0,0.3),
#'                              sd = 1,
#'                              labelnames = c("condition",
#'                              "cheerful", "neutral", "sad"))
#'
#' plot_power(design_result, max_n = 30)
#' @section References:
#' too be added
#' @import ggplot2
#' @export

plot_power <- function(design_result, max_n){
  design = design_result$design
  mu = design_result$mu
  sd <- design_result$sd
  r <- design_result$r

  #Do one ANOVA to get number of power columns
  length_power <- length(ANOVA_exact(design_result, verbose = FALSE)$main_results$power)
  power_df <- as.data.frame(matrix(0, ncol = length_power+1, nrow = max_n-7))
  power_df[,1] <- c(8:max_n)
  for (i in 1:(max_n-7)){
    design_result <- ANOVA_design(design = design,
                                  n = i+7,
                                  mu = mu,
                                  sd = sd,
                                  r = r,
                                  plot = FALSE)

    power_df[i,2:(1+length_power)] <- ANOVA_exact(design_result, verbose = FALSE)$main_results$power
  }
j=2
  for (j in 1:length_power){

    p1 <- ggplot(data=power_df, aes(x = power_df[,1], y = power_df[,(1+j)])) +
      geom_line( size=1.5) +
      scale_x_continuous(limits = c(0, max(max_n))) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_bw() +
      labs(x="Sample size", y = "Power")
    print(p1)
  }

  invisible(list(p1 = p1,
                 power_df = data.frame(paste("f = ",
                                             round(power_res$Cohen_f,2)),
                                       n_vec,
                                       power_A)))
}
