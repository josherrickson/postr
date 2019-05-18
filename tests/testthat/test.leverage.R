context("leverage")


## lm
data(mtcars)
m <- lm(hp ~ mpg, data = mtcars)

test_that("returned object is appropriate", {
  expect_is(postr_leverage(m), "numeric")
  expect_length(postr_leverage(m), nrow(mtcars))
})

## glm
m <- glm(vs ~ mpg, data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_is(postr_leverage(m), "numeric")
  expect_length(postr_leverage(m), nrow(mtcars))
})

## lme4
library(lme4)
m <- lmer(hp ~ mpg + (1|gear), data = mtcars)

test_that("returned object is appropriate", {
  expect_is(postr_leverage(m), "numeric")
  expect_length(postr_leverage(m), nrow(mtcars))
})

m <- glmer(vs ~ mpg + (1|gear), data = mtcars, family = binomial)

test_that("glmer error", {
  expect_error(postr_leverage(m), "not make sense")
  expect_error(postr_leverage(m), "bypass and compute")
})

test_that("returned object is appropriate", {
  expect_is(postr_leverage(m, force = TRUE), "numeric")
  expect_length(postr_leverage(m, force = TRUE), nrow(mtcars))
})

# Resetting model to something that doesn't require add'l arguments
m <- lm(hp ~ mpg, data = mtcars)

test_that("pr aliases", {
  expect_identical(postr_leverage(m), pr_leverage(m))
})

test_that("Improper object", {
  expect_error(pr_leverage(1), "leverage values not supported")
})
