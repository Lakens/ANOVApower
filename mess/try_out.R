library(afex)
library(MASS)
library(ggplot2)
library(reshape2)

string = "2w*2w"
n = 40
mu = c(1, 0, 1, 0)
sd = 2
r = 0.8
#labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot")

ANOVA_design(string = "2w*2w", n = 40, mu = c(1, 0, 1, 0), sd = 2, r = 0.8)


ANOVA_design(string = "2w*2w", n = 40, mu = c(1, 0, 1, 0), sd = 2, r = 0.8, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))
#Store factor levels (used many times in the script, calculate once)
factor_levels <- as.numeric(strsplit(string, "\\D+")[[1]])

#If labelnames are not provided, they are generated.
if (labelnames == NULL) {
  for(i1 in 1:length(factor_levels)) {
    labelnames <- paste(labelnames, paste(letters[i1]), sep = "")
    for(i2 in 1:factor_levels[i1]) {
      labelnames <- paste(labelnames, paste(letters[i1]), paste(i2), sep = "")
    }
  }
}
