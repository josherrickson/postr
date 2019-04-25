context("AUC")

test_that("returned object is appropriate", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_is(postr_AUC(m), "numeric")
  expect_length(postr_AUC(m), 1)
  expect_true(postr_AUC(m) <= 1)
  expect_true(postr_AUC(m) >= 0)

})

test_that("pr aliases", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_identical(postr_AUC(m), pr_AUC(m))
})
