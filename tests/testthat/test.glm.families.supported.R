context("glm family test")

## glm
data(mtcars)
m <- glm(vs ~ mpg, data = mtcars, family = binomial)

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

## glmer
library(lme4)
m <- glmer(vs ~ mpg + (1|cyl), data = mtcars, family = binomial)

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
