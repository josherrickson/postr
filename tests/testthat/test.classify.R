context("classify")

data(mtcars)
m <- glm(vs ~ mpg, data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_length(postr_classify(m, runif(1)), nrow(mtcars))
  expect_true(is.logical(postr_classify(m, runif(1))))
})

test_that("pr alias works", {
  r <- runif(1); expect_identical(postr_classify(m, r), pr_classify(m, r))
})

test_that("error on invalid threshold", {
  expect_error(pr_classify(m, -1))
  expect_error(pr_classify(m, 2))
})

test_that("Improper object", {
  expect_error(pr_classify(1, .5), "classify not supported")
  m <- update(m, family = poisson)
  expect_error(pr_classify(m, .5), "family must be")
})
