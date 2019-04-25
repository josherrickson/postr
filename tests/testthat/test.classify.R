context("classify")

test_that("returned object is appropriate", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_length(postr_classify(m, runif(1)), nrow(mtcars))
  expect_true(is.logical(postr_classify(m, runif(1))))
})

test_that("pr alias works", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  r <- runif(1); expect_identical(postr_classify(m, r), pr_classify(m, r))
})
