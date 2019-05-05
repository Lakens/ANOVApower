context("test-anova_design")

# error messages ----
test_that("errors", {
  # missing arguments
  expect_error(ANOVA_design(),
               "argument \"string\" is missing, with no default")
  expect_error(ANOVA_design("2w*2b"),
               "argument \"mu\" is missing, with no default")
  expect_error(ANOVA_design("2w*2b", mu = c(0, 0, 0, 0)),
               "argument \"sd\" is missing, with no default")
  expect_error(ANOVA_design("2w*2b", mu = c(0, 0, 0, 0), sd = 1),
               "argument \"n\" is missing, with no default")

  # bad arguments
  expect_error(ANOVA_design("wrong string"), "NA/NaN argument")
  #expect_error(ANOVA_design("2F", n = 10, mu = 1:2, sd = 1), "*SHOULD BE AN ERROR*")
  expect_error(ANOVA_design("2w*2b", n = "A", mu = 1:4, sd = 1),
               "non-numeric argument to binary operator")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = c("A", "B", "C", "D"), sd = 1),
               "non-numeric argument to binary operator")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = "A"),
               "requires numeric/complex matrix/vector arguments")
  #expect_error(ANOVA_design("2w*2b", n = 100, mu = c(0, 0, 0, 0), sd = -1), "?")
  #expect_error(ANOVA_design("2w*2b", n = c(10,10,10,10), mu = 1:4, sd = 1),
  #             "non-conformable arguments")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = 1, r = "A"),
               "object 'cor_mat' not found")

  # inconsistent arguments
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 0, sd = 1),
               "the length of the vector with means does not match the study design")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = c(1,1)),
               "the length of sd_for_sigma must be 1 or vars")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = 1, r = c(.5, 0, .4)),
               "object 'cor_mat' not found")
})

# test_that("2W", {
#   # fix uppercase letters are design = 0
#   d <- ANOVA_design("2W", n = 100, mu = c(0,0), sd = 1)
#   expect_equal(d$design, 1)
# })
#
# # test grep for strings ----
# test_that("test grep for strings", {
#   pattern <- "^([2-9](w|b)\\*){0,2}[2-9](w|b)$"
#
#   # should work
#   good_strings <- c("2w", "2b", "2W", "2B", "2w*2b", "3w*3b*2w")
#   for (x in good_strings) {
#     find <- grep(pattern, x, ignore.case = TRUE, perl = TRUE)
#     #expect_equal(find, 1)
#     if (length(find) == 0 || find != 1) { print(x) }
#   }
#
#
#   # should not work
#   bad_strings <- c("2a", "w2", "b2", "0w", "0b", "1w", "1b", "2b+2w", "2b*2b*2b*2b")
#   for (x in bad_strings) {
#     find <- grep(pattern, x, ignore.case = TRUE, perl = TRUE)
#     #expect_equal(find, integer(0))
#     if (length(find) != 0) { print(x) }
#   }
# })

# 2w defaults ----
test_that("2w defaults", {
  d <- ANOVA_design("2w", n = 100, mu = c(0,0), sd = 1, plot = FALSE)
  expect_equal(d$design, 1)
  expect_equal(d$design_list, c("a1", "a2"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ a + Error(subject/a))
  expect_equal(d$frml2, ~a)
  expect_equal(d$mu, c(0, 0))
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "a1" = c(1, 0),
    "a2" = c(0, 1),
    row.names = c("a1", "a2")
  )
  expect_equal(d$cor_mat, mat)
  expect_equal(d$sigmatrix, mat)

  expect_equal(d$string, "2w")
  expect_equal(d$labelnames, list(c("a1", "a2")))
  expect_equal(d$factornames, "a")
})

# 2b defaults----
test_that("2b defaults", {
  d <- ANOVA_design("2b", n = 100, mu = c(0,0), sd = 1, plot = FALSE)
  expect_equal(d$design, 0)
  expect_equal(d$design_list, c("a1", "a2"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ a + Error(1 | subject))
  expect_equal(d$frml2, ~a)
  expect_equal(d$mu, c(0, 0))
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "a1" = c(1, 0),
    "a2" = c(0, 1),
    row.names = c("a1", "a2")
  )
  expect_equal(d$cor_mat, mat)
  expect_equal(d$sigmatrix, mat)

  expect_equal(d$string, "2b")
  expect_equal(d$labelnames, list(c("a1", "a2")))
  expect_equal(d$factornames, "a")
})

# 2w set r & labels ----
test_that("2w set r & labels", {
  d <- ANOVA_design("2w", n = 100, mu = c(0,0), sd = 1, r = 0.5, labelnames = c("W", "W1", "W2"), plot = FALSE)
  expect_equal(d$design, 1)
  expect_equal(d$design_list, c("W1", "W2"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ W + Error(subject/W))
  expect_equal(d$frml2, ~W)
  expect_equal(d$mu, c(0, 0))
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0.5)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "W1" = c(1, 0.5),
    "W2" = c(0.5, 1),
    row.names = c("W1", "W2")
  )
  expect_equal(d$cor_mat, mat)
  expect_equal(d$sigmatrix, mat)

  expect_equal(d$string, "2w")
  expect_equal(d$labelnames, list(c("W1", "W2")))
  expect_equal(d$factornames, "W")
})

# 2b set r & labels----
test_that("2b set r & labels", {
  d <- ANOVA_design("2b", n = 100, mu = c(0,0), sd = 1, r = 0.5, labelnames = c("B", "B1", "B2"), plot = FALSE)
  expect_equal(d$design, 0)
  expect_equal(d$design_list, c("B1", "B2"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ B + Error(1 | subject))
  expect_equal(d$frml2, ~B)
  expect_equal(d$mu, c(0, 0))
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0.5)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "B1" = c(1, 0),
    "B2" = c(0, 1),
    row.names = c("B1", "B2")
  )
  expect_equal(d$cor_mat, mat)
  expect_equal(d$sigmatrix, mat)

  expect_equal(d$string, "2b")
  expect_equal(d$labelnames, list(c("B1", "B2")))
  expect_equal(d$factornames, "B")
})
