context("rates")

test_that("returned object is appropriate", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_length(gp_tpr(m, .5), 1)
  expect_is(gp_tpr(m, .5), "numeric")
  expect_is(gp_tnr(m, .5), "numeric")
  expect_is(gp_fpr(m, .5), "numeric")
  expect_is(gp_fnr(m, .5), "numeric")

})
