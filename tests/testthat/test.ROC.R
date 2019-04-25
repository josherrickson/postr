context("ROC")

data(mtcars)
m <- glm(vs ~ mpg, data = mtcars, family = binomial)

test_that("returned object is appropriate", {
  expect_is(postr_ROC(m), "gg")

})

test_that("pr aliases", {
  expect_equal(postr_ROC(m), pr_ROC(m))
  # gg objects (plots in general) aren't identical but are all.equal
})

test_that("Improper object", {
  expect_error(pr_ROC(1, .5))
  m <- update(m, family = poisson)
  expect_error(pr_ROC(m, .5))
})
