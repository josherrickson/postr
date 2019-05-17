context("VIF")


## lm
data(mtcars)
m <- lm(vs ~ mpg + hp + cyl, data = mtcars)

test_that("returned object is appropriate", {
  expect_is(postr_vif(m), "numeric")
  expect_length(postr_vif(m), 3)
})


## glm
data(mtcars)
m <- glm(vs ~ mpg + hp + cyl, data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_is(postr_vif(m), "numeric")
  expect_length(postr_vif(m), 3)
})


## lme4
library(lme4)
m <- lmer(hp ~ mpg + vs + cyl + (1|gear), data = mtcars)

test_that("returned object is appropriate", {
  expect_is(postr_vif(m), "numeric")
  expect_length(postr_vif(m), 3)
})

## glmer
m <- glmer(am ~ mpg +  cyl + (1|gear), data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_is(postr_vif(m), "numeric")
  expect_length(postr_vif(m), 2)
})


test_that("pr aliases", {
  expect_identical(postr_vif(m), pr_vif(m))
})

test_that("Improper object", {
  expect_error(pr_vif(1), "VIF not supported")
})
