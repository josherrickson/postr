context("rates")

test_that("returned object is appropriate", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_length(postr_tpr(m, .5), 1)
  expect_is(postr_tpr(m, .5), "numeric")
  expect_is(postr_tnr(m, .5), "numeric")
  expect_is(postr_fpr(m, .5), "numeric")
  expect_is(postr_fnr(m, .5), "numeric")

})

test_that("pr aliases", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_identical(postr_tpr(m, .5), pr_tpr(m, .5))
  expect_identical(postr_tnr(m, .5), pr_tnr(m, .5))
  expect_identical(postr_fpr(m, .5), pr_fpr(m, .5))
  expect_identical(postr_fnr(m, .5), pr_fnr(m, .5))

})
