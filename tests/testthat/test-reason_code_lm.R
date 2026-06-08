lm1 <- lm(mpg ~ cyl + wt, data = mtcars[-(1:2), ])
rc1 <- reason_code(lm1, mtcars[1:2, ])

testthat::test_that("reason_code.lm returns a reason_codes object", {
  testthat::expect_s3_class(rc1, "reason_codes")
})

testthat::test_that("predictions has one value per new observation", {
  testthat::expect_length(rc1$predictions, 2L)
})

testthat::test_that("contributions has correct dimensions", {
  testthat::expect_equal(nrow(rc1$contributions), 2L)
  # cyl and wt — intercept is dropped
  testthat::expect_equal(ncol(rc1$contributions), 2L)
  testthat::expect_named(rc1$contributions, c("cyl", "wt"))
})

testthat::test_that("contributions sum to prediction minus baseline for lm", {
  for (i in seq_along(rc1$predictions)) {
    total <- sum(rc1$contributions[i, ])
    testthat::expect_equal(
      unname(rc1$predictions[i]),
      rc1$baseline + total,
      tolerance = 1e-8
    )
  }
})

testthat::test_that("reasons data.frame has correct column names", {
  expected <- c("reason_1", "reason_1_contribution",
                "reason_2", "reason_2_contribution",
                "reason_3", "reason_3_contribution")
  testthat::expect_named(rc1$reasons, expected)
})

testthat::test_that("reason names are valid predictor names", {
  pred_names <- colnames(rc1$contributions)
  for (col in grep("^reason_[0-9]+$", names(rc1$reasons), value = TRUE)) {
    testthat::expect_true(all(rc1$reasons[[col]] %in% pred_names))
  }
})

testthat::test_that("n_reasons argument limits returned reason codes", {
  rc2 <- reason_code(lm1, mtcars[1:2, ], n_reasons = 1L)
  testthat::expect_named(rc2$reasons, c("reason_1", "reason_1_contribution"))
})

testthat::test_that("n_reasons is capped at the number of predictors", {
  rc_many <- reason_code(lm1, mtcars[1:2, ], n_reasons = 99L)
  # 2 predictors (cyl, wt) after dropping intercept
  testthat::expect_equal(ncol(rc_many$reasons), 4L)
})

testthat::test_that("as.data.frame returns a data.frame with prediction column", {
  df <- as.data.frame(rc1)
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_true("prediction" %in% names(df))
  testthat::expect_equal(nrow(df), 2L)
})

testthat::test_that("print.reason_codes runs without error", {
  testthat::expect_output(print(rc1), "Reason Codes for lm model")
})

testthat::test_that("summary.reason_codes runs without error", {
  testthat::expect_output(summary(rc1), "Reason Codes Summary")
})

testthat::test_that("single new observation works", {
  rc_single <- reason_code(lm1, mtcars[1, ])
  testthat::expect_length(rc_single$predictions, 1L)
  testthat::expect_equal(nrow(rc_single$contributions), 1L)
})

testthat::test_that("lm with multiple predictors ranks by absolute contribution", {
  lm2 <- lm(mpg ~ cyl + disp + hp + wt, data = mtcars[-(1:5), ])
  rc2 <- reason_code(lm2, mtcars[1:5, ], n_reasons = 4L)
  for (i in seq_len(nrow(rc2$reasons))) {
    abs_vals <- abs(as.numeric(
      rc2$reasons[i, grep("contribution", names(rc2$reasons))]
    ))
    testthat::expect_true(all(diff(abs_vals) <= 0))
  }
})

testthat::test_that("invalid n_reasons raises an error", {
  testthat::expect_error(reason_code(lm1, mtcars[1:2, ], n_reasons = 0L))
  testthat::expect_error(reason_code(lm1, mtcars[1:2, ], n_reasons = -1L))
})
