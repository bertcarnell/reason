testthat::skip_if_not_installed("survival")

library(survival)

data(veteran, package = "survival")
fit_cox <- survival::coxph(
  survival::Surv(time, status) ~ age + karno,
  data = veteran[-(1:5), ]
)
rc1 <- reason_code(fit_cox, veteran[1:5, ])

testthat::test_that("reason_code.coxph returns a reason_codes object", {
  testthat::expect_s3_class(rc1, "reason_codes")
})

testthat::test_that("predictions are positive hazard ratios", {
  testthat::expect_true(all(rc1$predictions > 0))
})

testthat::test_that("baseline is 1 (reference hazard ratio)", {
  testthat::expect_equal(rc1$baseline, 1.0)
})

testthat::test_that("predictions has one value per new observation", {
  testthat::expect_length(rc1$predictions, 5L)
})

testthat::test_that("contributions has correct dimensions", {
  testthat::expect_equal(nrow(rc1$contributions), 5L)
  testthat::expect_equal(ncol(rc1$contributions), 2L)
  testthat::expect_named(rc1$contributions, c("age", "karno"))
})

testthat::test_that("model_class is 'coxph'", {
  testthat::expect_equal(rc1$model_class, "coxph")
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

testthat::test_that("as.data.frame returns a data.frame with prediction column", {
  df <- as.data.frame(rc1)
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_true("prediction" %in% names(df))
  testthat::expect_equal(nrow(df), 5L)
})

testthat::test_that("print.reason_codes runs without error for coxph", {
  testthat::expect_output(print(rc1), "coxph")
})

testthat::test_that("summary.reason_codes runs without error for coxph", {
  testthat::expect_output(summary(rc1), "Reason Codes Summary")
})

testthat::test_that("single observation works", {
  rc_single <- reason_code(fit_cox, veteran[1L, ])
  testthat::expect_length(rc_single$predictions, 1L)
  testthat::expect_equal(nrow(rc_single$contributions), 1L)
})

testthat::test_that("contributions ranked by absolute value descending", {
  fit3 <- survival::coxph(
    survival::Surv(time, status) ~ age + karno + diagtime,
    data = veteran[-(1:5), ]
  )
  rc3 <- reason_code(fit3, veteran[1:5, ], n_reasons = 3L)
  for (i in seq_len(nrow(rc3$reasons))) {
    abs_vals <- abs(as.numeric(
      rc3$reasons[i, grep("contribution", names(rc3$reasons))]
    ))
    testthat::expect_true(all(diff(abs_vals) <= 0))
  }
})

testthat::test_that("n_reasons capped at number of predictors", {
  rc_many <- reason_code(fit_cox, veteran[1:5, ], n_reasons = 99L)
  testthat::expect_equal(ncol(rc_many$reasons), 4L)
})

testthat::test_that("invalid n_reasons raises an error", {
  testthat::expect_error(reason_code(fit_cox, veteran[1:5, ], n_reasons = 0L))
})
