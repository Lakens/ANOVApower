#' Simulation function used to perform the simulation
#' @param design_result Output from the ANOVA_design function
#' @param alpha_level Alpha level used to determine statistical significance
#' @param p_adjust Correction for multiple comparisons
#' @param nsims number of simulations to perform
#' @param verbose Set to FALSE to not print results (default = TRUE)
#' @return Returns dataframe with simulation data (p-values and effect sizes), anova results and simple effect results, plots of p-value distribution, p_adjust = p_adjust, nsims, and alpha_level.
#' @examples
#' ## Set up a within design with 2 factors, each with 2 levels,
#' ## with correlation between observations of 0.8,
#' ## 40 participants (woh do all conditions), and standard deviation of 2
#' ## with a mean pattern of 1, 0, 1, 0, conditions labeled 'condition' and
#' ## 'voice', with names for levels of "cheerful", "sad", amd "human", "robot"
#' design_result <- ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 1, 0),
#'       sd = 2, r = 0.8, labelnames = c("condition", "cheerful",
#'       "sad", "voice", "human", "robot"))
#' power_result <- ANOVA_power(design_result, alpha_level = 0.05,
#'       p_adjust = "none", nsims = 10)
#' @section References:
#' too be added
#' @importFrom stats pnorm pt qnorm qt as.formula median p.adjust
#' @importFrom utils combn
#' @importFrom reshape2 melt
#' @importFrom MASS mvrnorm
#' @importFrom afex aov_car
#' @import ggplot2
#' @export
#'

ANOVA_power <- function(design_result, alpha_level = 0.05, p_adjust = "none", nsims = 1000, verbose = TRUE){

  if (is.element(p_adjust, c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")) == FALSE ) {
    stop("p_adjust must be of an acceptable adjustment method: see ?p.adjust")
  }

  if (nsims < 10) {
    stop("The number of repetitions in simulation must be at least 10; suggested at least 1000 for accurate results")
  }

  options(scipen = 999) # 'turn off' scientific notation

  effect_size_d <- function(x, y, conf.level = 0.95){
    sd1 <- sd(x) #standard deviation of measurement 1
    sd2 <- sd(y) #standard deviation of measurement 2
    n1 <- length(x) #number of pairs
    n2 <- length(y) #number of pairs
    df <- n1 + n2 - 2
    m_diff <- mean(y - x)
    sd_pooled <- (sqrt((((n1 - 1) * ((sd1^2))) + (n2 - 1) * ((sd2^2))) / ((n1 + n2 - 2)))) #pooled standard deviation
    j <- (1 - 3/(4 * (n1 + n2 - 2) - 1)) #Calculate Hedges' correction.
    t_value <- m_diff / sqrt(sd_pooled^2 / n1 + sd_pooled^2 / n2)
    p_value = 2*pt(-abs(t_value), df = df)
    d <- m_diff / sd_pooled #Cohen's d
    d_unb <- d*j #Hedges g, of unbiased d

    invisible(list(d = d,
                   d_unb = d_unb,
                   p_value = p_value))
  }

  effect_size_d_paired <- function(x, y, conf.level = 0.95){
    sd1 <- sd(x) #standard deviation of measurement 1
    sd2 <- sd(y) #standard deviation of measurement 2
    s_diff <- sd(x - y) #standard deviation of the difference scores
    N <- length(x) #number of pairs
    df = N - 1
    s_av <- sqrt((sd1 ^ 2 + sd2 ^ 2) / 2) #averaged standard deviation of both measurements

    #Cohen's d_av, using s_av as standardizer
    m_diff <- mean(y - x)
    d_av <- m_diff / s_av
    d_av_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_av

    #get the t-value for the CI
    t_value <- m_diff / (s_diff / sqrt(N))
    p_value = 2 * pt(-abs(t_value), df = df)

    #Cohen's d_z, using s_diff as standardizer
    d_z <- t_value / sqrt(N)
    d_z_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_z

    invisible(list(
      d_z = d_z,
      d_z_unb = d_z_unb,
      p_value = p_value
    ))
  }

  round_dig <- 4 #Set digits to which you want to round the output.

  if (missing(alpha_level)) {
    alpha_level <- 0.05
  }

  if (alpha_level >= 1 | alpha_level <= 0  ) {
    stop("alpha_level must be less than 1 and greater than zero")
  }

  ###############
  # 2. Read in Environment Data ----
  ###############

  string <- design_result$string #String used to specify the design
  factornames <- design_result$factornames #Get factor names
  n <- design_result$n
  mu = design_result$mu # population means - should match up with the design
  sd <- design_result$sd #population standard deviation (currently assumes equal variances)
  r <- design_result$r # correlation between within factors (currently only 1 value can be entered)
  factors <- design_result$factors
  design <- design_result$design
  sigmatrix <- design_result$sigmatrix
  dataframe <- design_result$dataframe
  design_list <- design_result$design_list

    ###############
  # 3. Specify factors for formula ----
  ###############

  frml1 <- design_result$frml1
  frml2 <- design_result$frml2

  aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter formula 1 as designed above on the basis of the design
                                         data = dataframe, include_aov = FALSE,
                                         anova_table = list(es = "pes", p_adjust_method = p_adjust)) }) #This reports PES not GES

  ###############
  # 5. Set up dataframe for simulation results
  ###############

  #How many possible planned comparisons are there (to store p and es)
  possible_pc <- (((prod(
    as.numeric(strsplit(design, "\\D+")[[1]])
  )) ^ 2) - prod(as.numeric(strsplit(design, "\\D+")[[1]])))/2

  #create empty dataframe to store simulation results
  #number of columns for ANOVA results and planned comparisons, times 2 (p-values and effect sizes)
  sim_data <- as.data.frame(matrix(
    ncol = 2 * (2 ^ factors - 1) + 2 * possible_pc,
    nrow = nsims
  ))

  paired_tests <- combn(unique(dataframe$cond),2)
  paired_p <- numeric(possible_pc)
  paired_d <- numeric(possible_pc)
  within_between <- sigmatrix[lower.tri(sigmatrix)] #based on whether correlation is 0 or not, we can determine if we should run a paired or independent t-test

  #Dynamically create names for the data we will store
  names(sim_data) = c(paste("anova_",
                            rownames(aov_result$anova_table),
                            sep = ""),
                      paste("anova_es_",
                            rownames(aov_result$anova_table),
                            sep = ""),
                      paste("p_",
                            paste(paired_tests[1,],paired_tests[2,],sep = "_"),
                            sep = ""),
                      paste("d_",
                            paste(paired_tests[1,],paired_tests[2,], sep = "_"),
                            sep = ""))


  ###############
  # 7. Start Simulation ----
  ###############
  #withProgress(message = 'Running simulations', value = 0, { #block outside of Shiny
    for (i in 1:nsims) { #for each simulated experiment
      #incProgress(1/nsims, detail = paste("Now running simulation", i, "out of",nsims,"simulations")) #Block outside of Shiny
      #We simulate a new y variable, melt it in long format, and add it to the dataframe (surpressing messages)
      dataframe$y <- suppressMessages({
        melt(as.data.frame(mvrnorm(
          n = n,
          mu = mu,
          Sigma = as.matrix(sigmatrix)
        )))$value
      })

      # We perform the ANOVA using AFEX
      #Can be set to NICE to speed up, but required data grabbing from output the change.
      aov_result <- suppressMessages({aov_car(frml1, #here we use frml1 to enter fromula 1 as designed above on the basis of the design
                                            data = dataframe, include_aov = FALSE, #Need development code to get aov_include function
                                            anova_table = list(es = "pes",
                                                               p_adjust_method = p_adjust))}) #This reports PES not GES

      for (j in 1:possible_pc) {
        x <- dataframe$y[which(dataframe$cond == paired_tests[1,j])]
        y <- dataframe$y[which(dataframe$cond == paired_tests[2,j])]
        #this can be sped up by tweaking the functions that are loaded to only give p and dz
        ifelse(within_between[j] == 0,
               t_test_res <- effect_size_d(x, y, conf.level = 1 - alpha_level),
               t_test_res <- effect_size_d_paired(x, y, conf.level = 1 - alpha_level))
        paired_p[j] <- t_test_res$p_value
        paired_d[j] <- ifelse(within_between[j] == 0,
                                    t_test_res$d,
                                    t_test_res$d_z)
        }

      # store p-values and effect sizes for calculations and plots.
      sim_data[i,] <- c(aov_result$anova_table[[6]], #p-value for ANOVA
                        aov_result$anova_table[[5]], #partial eta squared
                        p.adjust(paired_p, method = p_adjust), #p-values for paired comparisons, added correction for multiple comparisons
                        paired_d #effect sizes
    )}
  #}) #close withProgress Block outside of Shiny

  ############################################
  #End Simulation              ###############


  ###############
  # 8. Plot Results ----
  ###############

  # melt the data into a long format for plots in ggplot2

  plotData <- suppressMessages(melt(sim_data[1:(2 ^ factors - 1)], value.name = 'p'))

  SalientLineColor <- "#535353"
  LineColor <- "Black"
  BackgroundColor <- "White"

  # plot each of the p-value distributions
  #create variable p to use in ggplot and prevent package check error.
  p <- plotData$p
  # Helper function for string wrapping.
  swr = function(string, nwrap=10) {
    paste(strwrap(string, width=10), collapse="\n")
  }
  swr = Vectorize(swr)

  # Create line breaks in variable
  plotData$variable = swr(chartr("_:", "  ", plotData$variable))

  plt1 = ggplot(plotData, aes(x = p)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1),
                       labels = seq(0, 1, by = .1)) +
    geom_histogram(colour = "black",
                   fill = "white",
                   breaks = seq(0, 1, by = .01)) +
    geom_vline(xintercept = alpha_level, colour = 'red') +
    facet_grid(variable ~ .) +
    labs(x = "p") +
    theme_bw()
  #Plot p-value distributions for simple comparisons
  # melt the data into a ggplot friendly 'long' format
  p_paired <- sim_data[(2 * (2 ^ factors - 1) + 1):(2 * (2 ^ factors - 1) + possible_pc)]

  plotData <- suppressMessages(melt(p_paired, value.name = 'p'))
  #create variable p to use in ggplot and prevent package check error.
  p <- plotData$p
  # Create line breaks in variable
  plotData$variable = swr(chartr("_:", "  ", plotData$variable))

  # plot each of the p-value distributions
  plt2 = ggplot(plotData, aes(x = p)) +
    scale_x_continuous(breaks = seq(0, 1, by = .1),
                       labels = seq(0, 1, by = .1)) +
    geom_histogram(colour = "black",
                   fill = "white",
                   breaks = seq(0, 1, by = .01)) +
    geom_vline(xintercept = alpha_level, colour = 'red') +
    facet_grid(variable ~ .) +
    labs(x = expression(p)) +
    theme_bw()
  ###############
  # 9. Sumary of power and effect sizes of main effects and contrasts ----
  ###############

  #Main effects and interactions from the ANOVA
  power = as.data.frame(apply(as.matrix(sim_data[(1:(2 ^ factors - 1))]), 2,
                              function(x) round(mean(ifelse(x < alpha_level, 1, 0) * 100),round_dig)))

  es = as.data.frame(apply(as.matrix(sim_data[((2^factors):(2 * (2 ^ factors - 1)))]), 2,
                           function(x) round(median(x),round_dig)))

  main_results <- data.frame(power,es)
  names(main_results) = c("power","effect_size")

  #Data summary for pairwise comparisons
  power_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + 1):(2 * (2 ^ factors - 1) + possible_pc)]), 2,
                                     function(x) round(mean(ifelse(x < alpha_level, 1, 0) * 100),round_dig)))

  es_paired = as.data.frame(apply(as.matrix(sim_data[(2 * (2 ^ factors - 1) + possible_pc + 1):(2*(2 ^ factors - 1) + 2 * possible_pc)]), 2,
                                  function(x) round(mean(x),round_dig)))

  pc_results <- data.frame(power_paired, es_paired)
  names(pc_results) = c("power","effect_size")

  #######################
  # Return Results ----
  #######################
  if(verbose == TRUE){
    # The section below should be blocked out when in Shiny
    cat("Power and Effect sizes for ANOVA tests")
    cat("\n")
    print(main_results)
    cat("\n")
    cat("Power and Effect sizes for contrasts")
    cat("\n")
    print(pc_results)
  }

  # Return results in list()
  invisible(list(sim_data = sim_data,
                 main_results = main_results,
                 pc_results = pc_results,
                 plot1 = plt1,
                 plot2 = plt2,
                 p_adjust = p_adjust,
                 nsims = nsims,
                 alpha_level = alpha_level))
}
