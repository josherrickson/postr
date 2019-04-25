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
