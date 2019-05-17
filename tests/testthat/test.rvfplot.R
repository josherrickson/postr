context("RVF plot")

## lm
data(mtcars)
m <- lm(vs ~ mpg, data = mtcars)

test_that("returned object is appropriate", {
  expect_is(postr_rvfplot(m), "gg")
})

## glm
m <- glm(hp ~ mpg, data = mtcars, family = gaussian)

test_that("returned object is appropriate", {
  expect_is(postr_rvfplot(m), "gg")
})

test_that("pr aliases", {
  expect_equal(postr_rvfplot(m), pr_rvfplot(m))
  # gg objects (plots in general) aren't identical but are all.equal
})

test_that("Improper object", {
  expect_error(pr_rvfplot(1, .5), "RVF plot not supported")
})
