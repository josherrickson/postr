context("glm family test")

m <- "abc"
class(m) <- "glm"
suppressWarnings(m$family <- 1)
suppressWarnings(m$family$family <- "binomial")

test_that("matches correct family", {
  expect_true(.glm.families.supported(m, "binomial"))
})

test_that("matches if family in list", {
  expect_true(.glm.families.supported(m, "poisson", "binomial"))
})

test_that("failure to match", {
  expect_error(.glm.families.supported(m, "poisson"),
               "glm family must be poisson")
  expect_error(.glm.families.supported(m, "poisson", "gamma"),
               "glm family must be one of poisson, gamma")
})
