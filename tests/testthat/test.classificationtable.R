context("classtable")

## glm
data(mtcars)
m <- glm(vs ~ mpg, data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_is(postr_classificationtable(m, runif(1)), "table")
})

test_that("dimensions are always 2x2", {
  expect_equal(dim(postr_classificationtable(m, runif(1))), c(2,2))
  expect_equal(dim(postr_classificationtable(m, 1)), c(2,2))
  expect_equal(dim(postr_classificationtable(m, 0)), c(2,2))
})

## glmer
library(lme4)
m <- glmer(vs ~ mpg + (1 | gear), data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_is(postr_classificationtable(m, runif(1)), "table")
})

test_that("dimensions are always 2x2", {
  expect_equal(dim(postr_classificationtable(m, runif(1))), c(2,2))
  expect_equal(dim(postr_classificationtable(m, 1)), c(2,2))
  expect_equal(dim(postr_classificationtable(m, 0)), c(2,2))
})

test_that("pr aliases", {
  r <- runif(1); expect_identical(postr_classificationtable(m, r),
                                  postr_classtable(m, r))
  r <- runif(1); expect_identical(postr_classificationtable(m, r),
                                  pr_classificationtable(m, r))
  r <- runif(1); expect_identical(postr_classificationtable(m, r),
                                  pr_classtable(m, r))
})

test_that("Improper object", {
  expect_error(postr_classificationtable(1, .5), "table not supported")
  m <- update(m, family = poisson)
  expect_error(postr_classificationtable(m, .5), "family must be")
})
