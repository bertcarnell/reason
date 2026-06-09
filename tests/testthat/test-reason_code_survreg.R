testthat::skip_if_not_installed("survival")

library(survival)

data(veteran, package = "survival")
fit_surv <- survival::survreg(
  survival::Surv(time, status) ~ age + karno,
  data = veteran[-(1:5), ],
  model = TRUE
)
rc1 <- reason_code(fit_surv, veteran[1:5, ])

testthat::test_that("reason_code.survreg returns a reason_codes object", {
  testthat::expect_s3_class(rc1, "reason_codes")
})

testthat::test_that("predictions are positive survival times", {
  testthat::expect_true(all(rc1$predictions > 0))
})

testthat::test_that("baseline is a positive survival time", {
  testthat::expect_true(rc1$baseline > 0)
})

testthat::test_that("predictions has one value per new observation", {
  testthat::expect_length(rc1$predictions, 5L)
})

testthat::test_that("contributions has correct dimensions", {
  testthat::expect_equal(nrow(rc1$contributions), 5L)
  testthat::expect_equal(ncol(rc1$contributions), 2L)
  testthat::expect_named(rc1$contributions, c("age", "karno"))
})

testthat::test_that("model_class includes distribution name", {
  testthat::expect_match(rc1$model_class, "survreg")
  testthat::expect_match(rc1$model_class, "weibull")
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

testthat::test_that("print.reason_codes runs without error for survreg", {
  testthat::expect_output(print(rc1), "survreg")
})

testthat::test_that("summary.reason_codes runs without error for survreg", {
  testthat::expect_output(summary(rc1), "Reason Codes Summary")
})

testthat::test_that("train_data argument works as alternative to model = TRUE", {
  fit_no_model <- survival::survreg(
    survival::Surv(time, status) ~ age + karno,
    data = veteran[-(1:5), ]   # no model = TRUE
  )
  rc_td <- reason_code(fit_no_model, veteran[1:5, ],
                       train_data = veteran[-(1:5), ])
  testthat::expect_s3_class(rc_td, "reason_codes")
  testthat::expect_length(rc_td$predictions, 5L)
})

testthat::test_that("missing training data raises an error", {
  fit_no_model <- survival::survreg(
    survival::Surv(time, status) ~ age + karno,
    data = veteran[-(1:5), ]
  )
  testthat::expect_error(
    reason_code(fit_no_model, veteran[1:5, ]),
    "train_data"
  )
})

testthat::test_that("single observation works", {
  rc_single <- reason_code(fit_surv, veteran[1L, ])
  testthat::expect_length(rc_single$predictions, 1L)
  testthat::expect_equal(nrow(rc_single$contributions), 1L)
})

testthat::test_that("contributions ranked by absolute value descending", {
  fit3 <- survival::survreg(
    survival::Surv(time, status) ~ age + karno + diagtime,
    data = veteran[-(1:5), ], model = TRUE
  )
  rc3 <- reason_code(fit3, veteran[1:5, ], n_reasons = 3L)
  for (i in seq_len(nrow(rc3$reasons))) {
    abs_vals <- abs(as.numeric(
      rc3$reasons[i, grep("contribution", names(rc3$reasons))]
    ))
    testthat::expect_true(all(diff(abs_vals) <= 0))
  }
})

testthat::test_that("lognormal distribution works", {
  fit_lnorm <- survival::survreg(
    survival::Surv(time, status) ~ age + karno,
    data = veteran[-(1:5), ], dist = "lognormal", model = TRUE
  )
  rc_lnorm <- reason_code(fit_lnorm, veteran[1:5, ])
  testthat::expect_s3_class(rc_lnorm, "reason_codes")
  testthat::expect_match(rc_lnorm$model_class, "lognormal")
  testthat::expect_true(all(rc_lnorm$predictions > 0))
})

testthat::test_that("invalid n_reasons raises an error", {
  testthat::expect_error(reason_code(fit_surv, veteran[1:5, ], n_reasons = 0L))
})
