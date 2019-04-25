context("AUC")

data(mtcars)
m <- glm(vs ~ mpg, data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_is(postr_AUC(m), "numeric")
  expect_length(postr_AUC(m), 1)
  expect_true(postr_AUC(m) <= 1)
  expect_true(postr_AUC(m) >= 0)

})

test_that("pr aliases", {
  expect_identical(postr_AUC(m), pr_AUC(m))
})

test_that("Improper object", {
  expect_error(pr_AUC(1, .5))
  m <- update(m, family = poisson)
  expect_error(pr_AUC(m, .5))
})
