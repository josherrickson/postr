context("ROC")

test_that("returned object is appropriate", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_is(postr_ROC(m), "gg")

})

test_that("pr aliases", {
  data(mtcars)
  m <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_equal(postr_ROC(m), pr_ROC(m))
  # gg objects (plots in general) aren't identical but are all.equal
})
