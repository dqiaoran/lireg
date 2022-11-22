test_that("summary.lireg", {
  f = mtcars$mpg ~ mtcars$am
  expect_equal(summary(lireg(f))$coefficients, summary(lm(f))$coefficients)
})
