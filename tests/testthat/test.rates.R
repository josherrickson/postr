context("rates")

test_that("returned object is appropriate", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_length(postr_tpr(m, runif(1)), 1)
  expect_is(postr_tpr(m, runif(1)), "numeric")
  expect_is(postr_tnr(m, runif(1)), "numeric")
  expect_is(postr_fpr(m, runif(1)), "numeric")
  expect_is(postr_fnr(m, runif(1)), "numeric")

})

test_that("pr aliases", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  r <- runif(1); expect_identical(postr_tpr(m, r), pr_tpr(m, r))
  r <- runif(1); expect_identical(postr_tnr(m, r), pr_tnr(m, r))
  r <- runif(1); expect_identical(postr_fpr(m, r), pr_fpr(m, r))
  r <- runif(1); expect_identical(postr_fnr(m, r), pr_fnr(m, r))

})

test_that("sens and spec aliases", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  r <- runif(1); expect_identical(postr_tpr(m, r), postr_sensitivity(m, r))
  r <- runif(1); expect_identical(postr_tpr(m, r), pr_sensitivity(m, r))
  r <- runif(1); expect_identical(postr_tnr(m, r), postr_specificity(m, r))
  r <- runif(1); expect_identical(postr_tnr(m, r), pr_specificity(m, r))
})

test_that("rates at thresholds 1 and 0", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_equal(pr_tpr(m, 0), 1)
  expect_equal(pr_tnr(m, 0), 0)
  expect_equal(pr_fpr(m, 0), 1)
  expect_equal(pr_fnr(m, 0), 0)
})
