context("observed")


## glm
data(mtcars)
m <- glm(vs ~ mpg, data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_is(postr_observed(m), "numeric")
  expect_length(postr_observed(m), nrow(mtcars))
  expect_identical(postr_observed(m), mtcars$vs)
})

## lm
m <- lm(hp ~ mpg, data = mtcars)

test_that("returned object is appropriate", {
  expect_is(postr_observed(m), "numeric")
  expect_length(postr_observed(m), nrow(mtcars))
  expect_identical(postr_observed(m), mtcars$hp)
})

## lme4
library(lme4)
m <- lmer(hp ~ mpg + (1|gear), data = mtcars)

test_that("returned object is appropriate", {
  expect_is(postr_observed(m), "numeric")
  expect_length(postr_observed(m), nrow(mtcars))
  expect_identical(postr_observed(m), mtcars$hp)
})

m <- glmer(vs ~ mpg + (1|gear), data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_is(postr_observed(m), "numeric")
  expect_length(postr_observed(m), nrow(mtcars))
  expect_identical(postr_observed(m), mtcars$vs)
})


test_that("pr aliases", {
  expect_identical(postr_observed(m), pr_observed(m))
})

test_that("Improper object", {
  expect_error(pr_observed(1), "observed values not supported")
})
