context("classify")

test_that("returned object is appropriate", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_length(gp_classify(m, .5), nrow(mtcars))
  expect_true(is.logical(gp_classify(m, .5)))
})
