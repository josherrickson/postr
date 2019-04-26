context("classtable")

data(mtcars)
m <- glm(vs ~ mpg, data = mtcars, family = binomial)


test_that("returned object is appropriate", {
  expect_is(postr_classtable(m, runif(1)), "table")

})

test_that("dimensions are always 2x2", {
  expect_equal(dim(postr_classtable(m, runif(1))), c(2,2))
  expect_equal(dim(postr_classtable(m, 1)), c(2,2))
  expect_equal(dim(postr_classtable(m, 0)), c(2,2))
})


test_that("Improper object", {
  expect_error(pr_classtable(1, .5), "table not supported")
  m <- update(m, family = poisson)
  expect_error(pr_classtable(m, .5), "family must be")
})

