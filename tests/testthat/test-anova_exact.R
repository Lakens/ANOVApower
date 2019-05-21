context("test-anova_exact")



# error messages
test_that("error messages", {
  design <- ANOVA_design(string = "2w", n = 10, mu = c(0, 0), sd = 1, plot = FALSE)

  expect_error(ANOVA_exact(), "argument \"design_result\" is missing, with no default")

})


#2w null
test_that("2w null", {
  design <- ANOVA_design(string = "2w", n = 100, mu = c(0, 0), sd = 1, r = 0.5, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 5)
  expect_equal(p$pc_results$power, 5)

})

#2b null
test_that("2b null", {
  design <- ANOVA_design(string = "2b", n = 100, mu = c(0, 0), sd = 1, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 5)
  expect_equal(p$pc_results$power, 5)

})


#2w moderate effect
test_that("2w", {
  design <- ANOVA_design(string = "2w", n = 21, mu = c(0, 0.65), sd = 1, r = 0.55, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 84.7, tolerance = 0.1)
  expect_equal(p$pc_results$power, 84.7, tolerance = 0.1)

})

#2b moderate effect
test_that("2b", {
  design <- ANOVA_design(string = "2b", n = 22, mu = c(0, 0.65), sd = 1, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 55.8, tolerance = 0.1)
  expect_equal(p$pc_results$power, 55.8, tolerance = 0.1)

})

#3w null
test_that("3w null", {
  design <- ANOVA_design(string = "3w", n = 100,
                         mu = c(0, 0, 0), sd = 1, r = 0.5, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 5)
  expect_equal(p$pc_results$power, c(5,5,5))

})

#4b low power
test_that("4b", {
  design <- ANOVA_design(string = "4b", n = 15,
                         mu = c(0, 0.25, 0.33, 0.44),
                         sd = 1, plot = FALSE)

  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 15, tolerance = 0.1)
  expect_equal(p$pc_results$power,
               c(10.14,14.08,21.39,5.51,7.94,5.98),
               tolerance = 0.1)

})

