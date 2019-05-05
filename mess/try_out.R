library(afex)
library(MASS)
library(ggplot2)
library(reshape2)
library(ANOVApower)

devtools::install_github("Lakens/ANOVApower")
devtools::build_vignettes()
devtools::document()
devtools::build_manual()


string = "2b*2w"
n = 40
mu = c(1, 0, 1, 0)
sd = 2
r = 0.8
labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot")

design_result_new <- ANOVA_design(string = "2b*2w", n = 40, mu = c(0, 0, 0, 0), sd = 2, r = 0.8, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))
design_result_new$cor_mat

power_result <- ANOVA_power(design_result_new, alpha_level = 0.05,
                            p_adjust = "none", nsims = 1000)

power_result$plot1

design_result <- ANOVA_design(string = "2w*2b", n = 40, mu = c(0, 0, 0, 0), sd = 2, r = 0.8, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))
design_result$cor_mat

power_result <- ANOVA_power(design_result, alpha_level = 0.05,
                            p_adjust = "none", nsims = 1000)

power_result$plot1

design_result_new$frml1 == design_result_old$frml1
design_result_new$frml2 == design_result_old$frml2

design_result_new$cor_mat == design_result_old$cor_mat

xxx <- data.frame(matrix(NA, nrow = prod(factor_levels), ncol = 0))
for(j in 1:factors){
  xxx <- cbind(xxx, as.factor(unlist(rep(as.list(paste(labelnameslist[[j]],
                                                       sep="_")),
                                         each = prod(factor_levels)/prod(factor_levels[1:j]),
                                         times = prod(factor_levels)/prod(factor_levels[j:factors])
  ))))
}
xxx$cond <- as.character(interaction(xxx[, 1:factors], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)


xxx <- data.frame(matrix(NA, nrow = prod(factor_levels), ncol = 0))
for(j in 1:factors){
  xxx <- cbind(xxx, labelnameslist[[j]])
}
xxx$cond <- as.character(interaction(xxx[, 1:factors], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)

