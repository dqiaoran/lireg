test_that("check coefficients", {
  f = mtcars$mpg ~ mtcars$am
  expect_equal(lireg(f)$coefficients, lm(f)$coefficients)
})
