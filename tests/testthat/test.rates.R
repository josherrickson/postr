context("rates")

data(mtcars)
m <- glm(vs ~ mpg, data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_length(postr_tpr(m, runif(1)), 1)
  expect_is(postr_tpr(m, runif(1)), "numeric")
  expect_is(postr_tnr(m, runif(1)), "numeric")
  expect_is(postr_fpr(m, runif(1)), "numeric")
  expect_is(postr_fnr(m, runif(1)), "numeric")

})

test_that("pr aliases", {
  r <- runif(1); expect_identical(postr_tpr(m, r), pr_tpr(m, r))
  r <- runif(1); expect_identical(postr_tnr(m, r), pr_tnr(m, r))
  r <- runif(1); expect_identical(postr_fpr(m, r), pr_fpr(m, r))
  r <- runif(1); expect_identical(postr_fnr(m, r), pr_fnr(m, r))

})

test_that("sens and spec aliases", {
  r <- runif(1); expect_identical(postr_tpr(m, r), postr_sensitivity(m, r))
  r <- runif(1); expect_identical(postr_tpr(m, r), pr_sensitivity(m, r))
  r <- runif(1); expect_identical(postr_tnr(m, r), postr_specificity(m, r))
  r <- runif(1); expect_identical(postr_tnr(m, r), pr_specificity(m, r))
})

test_that("rates at thresholds 1 and 0", {
  expect_equal(pr_tpr(m, 0), 1)
  expect_equal(pr_tnr(m, 0), 0)
  expect_equal(pr_fpr(m, 0), 1)
  expect_equal(pr_fnr(m, 0), 0)
})

test_that("rates at thresholds 1 and 0", {
  expect_equal(pr_tpr(m, 1), 0)
  expect_equal(pr_tnr(m, 1), 1)
  expect_equal(pr_fpr(m, 1), 0)
  expect_equal(pr_fnr(m, 1), 1)
})

test_that("improper object", {
  expect_error(pr_tpr(1, .5), "model must be")
  expect_error(pr_tnr(1, .5), "model must be")
  expect_error(pr_fpr(1, .5), "model must be")
  expect_error(pr_fnr(1, .5), "model must be")
  m <- update(m, family = poisson)
  expect_error(pr_tpr(m, .5), "family must be")
  expect_error(pr_tnr(m, .5), "family must be")
  expect_error(pr_fpr(m, .5), "family must be")
  expect_error(pr_fnr(m, .5), "family must be")
})
