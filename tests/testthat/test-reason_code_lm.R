testthat::test_that("Reason code works for linear models", {
  lm1 <- lm(mpg ~ cyl + wt, data = mtcars[-(1:2),])
  rc1 <- reason_code(lm1, mtcars[1:2,])
  testthat::expect_equal(length(rc1), 2)
})
