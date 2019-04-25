context("classify")

test_that("returned object is appropriate", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_length(postr_classify(m, .5), nrow(mtcars))
  expect_true(is.logical(postr_classify(m, .5)))
})

test_that("pr alias works", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_identical(postr_classify(m, .5), pr_classify(m, .5))
})
