context("test-anova_power")



# error messages
test_that("error messages", {
  design <- ANOVA_design(string = "2w", n = 10, mu = c(0, 0), sd = 1, plot = FALSE)

  expect_error(ANOVA_power(), "argument \"design_result\" is missing, with no default")
  expect_error(ANOVA_power(design, nsims = -1), "The number of repetitions in simulation must be at least 10; suggested at least 1000 for accurate results")
  expect_error(ANOVA_power(design, nsims = 10, p_adjust = "BEEFERONNI"), "p_adjust must be of an acceptable adjustment method: see ?p.adjust",
               fixed = TRUE)
})

#2w
test_that("2w", {
  design <- ANOVA_design(string = "2w", n = 100, mu = c(0, 0.25), sd = 1, r = 0.5, plot = FALSE)
  p <- ANOVA_power(design, nsims = 50, seed = 8675309, verbose = FALSE)

  comp <- list()
  comp$main_results <- data.frame(
    power = c(68),
    effect_size = c(0.0563),
    row.names = c("anova_a")
  )

  comp$pc_results <- data.frame(
    power = c(68),
    effect_size = c(0.2518),
    row.names = c("p_a_a1_a_a2")
  )

    expect_equal(p$main_results, comp$main_results)
    expect_equal(p$pc_results, comp$pc_results)
    expect_equal(p$p_adjust, "none")
    expect_equal(p$nsims, 50)
})

# 2w*2w
test_that("2w*2w", {
  design <- ANOVA_design(string = "2w*2w", n = 40, mu = c(1, 0, 1, 0), sd = 2, r = 0.8,
                         labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"),
                         plot = FALSE)

  p <- ANOVA_power(design, nsims = 50, seed = 8675309, verbose = FALSE)

  comp <- list()
  comp$main_results <- data.frame(
    power = c(6, 100, 6),
    effect_size = c(0.0125, 0.5402, 0.0144),
    row.names = c("anova_condition", "anova_voice", "anova_condition:voice")
  )

  comp$pc_results <- data.frame(
    power = c(100, 4, 100, 100, 4, 100),
    effect_size = c(-0.7665, 0.0095, -0.7500, 0.8036, 0.0244, -0.7950),
    row.names = c(
      "p_condition_cheerful_voice_human_condition_cheerful_voice_robot",
      "p_condition_cheerful_voice_human_condition_sad_voice_human",
      "p_condition_cheerful_voice_human_condition_sad_voice_robot",
      "p_condition_cheerful_voice_robot_condition_sad_voice_human",
      "p_condition_cheerful_voice_robot_condition_sad_voice_robot",
      "p_condition_sad_voice_human_condition_sad_voice_robot"
    )
  )

  expect_equal(p$main_results, comp$main_results)
  expect_equal(p$pc_results, comp$pc_results)
  expect_equal(p$p_adjust, "none")
  expect_equal(p$nsims, 50)
})

#2w long
test_that("2w long", {
  skip_on_cran()

  design <- ANOVA_design(string = "2w", n = 100, mu = c(0, 0.25), sd = 1, r = 0.5, plot = FALSE)
  system.time(
    p <- ANOVA_power(design, nsims = 1000, seed = 8675309, verbose = FALSE)
  )

  p2 <- pwr::pwr.t.test(n = 100, d = 0.25, type = "paired")

  expect_equal(p$main_results$power/100, p2$power, tolerance = .02)
  expect_equal(p$pc_results$power/100, p2$power, tolerance = .02)
  expect_equal(p$pc_results$effect_size, p2$d, tolerance = .02)
})
