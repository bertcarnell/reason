testthat::skip_if_not_installed("glmnet")

library(glmnet)

x_train <- as.matrix(mtcars[-(1:2), c("cyl", "wt")])
y_train <- mtcars[-(1:2), "mpg"]
x_new   <- as.matrix(mtcars[1:2, c("cyl", "wt")])
fit_gauss <- glmnet::glmnet(x_train, y_train, alpha = 1, family = "gaussian")
s_val     <- fit_gauss$lambda[10]
rc1       <- reason_code(fit_gauss, x_new, x_train = x_train, s = s_val)

testthat::test_that("reason_code.glmnet returns a reason_codes object", {
  testthat::expect_s3_class(rc1, "reason_codes")
})

testthat::test_that("predictions has one value per new observation", {
  testthat::expect_length(rc1$predictions, 2L)
})

testthat::test_that("contributions has correct dimensions", {
  testthat::expect_equal(nrow(rc1$contributions), 2L)
  testthat::expect_equal(ncol(rc1$contributions), 2L)
  testthat::expect_named(rc1$contributions, c("cyl", "wt"))
})

testthat::test_that("model_class includes family label", {
  testthat::expect_match(rc1$model_class, "glmnet")
  testthat::expect_match(rc1$model_class, "gaussian")
})

testthat::test_that("reasons data.frame has correct column names for 2 predictors", {
  expected <- c("reason_1", "reason_1_contribution",
                "reason_2", "reason_2_contribution")
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
  testthat::expect_equal(nrow(df), 2L)
})

testthat::test_that("print.reason_codes runs without error for glmnet", {
  testthat::expect_output(print(rc1), "glmnet")
})

testthat::test_that("summary.reason_codes runs without error for glmnet", {
  testthat::expect_output(summary(rc1), "Reason Codes Summary")
})

testthat::test_that("binomial glmnet predictions are probabilities", {
  x_tr2 <- as.matrix(mtcars[-(1:2), c("cyl", "wt")])
  y_tr2 <- mtcars[-(1:2), "am"]
  fit_bin <- glmnet::glmnet(x_tr2, y_tr2, alpha = 1, family = "binomial")
  rc_bin  <- reason_code(fit_bin, x_new,
                         x_train = x_tr2, s = fit_bin$lambda[5])
  testthat::expect_s3_class(rc_bin, "reason_codes")
  testthat::expect_true(all(rc_bin$predictions >= 0 & rc_bin$predictions <= 1))
  testthat::expect_true(rc_bin$baseline >= 0 && rc_bin$baseline <= 1)
  testthat::expect_match(rc_bin$model_class, "binomial")
})

testthat::test_that("n_reasons is capped at number of predictors", {
  rc_many <- reason_code(fit_gauss, x_new, x_train = x_train,
                         s = s_val, n_reasons = 99L)
  testthat::expect_equal(ncol(rc_many$reasons), 4L)
})

testthat::test_that("contributions ranked by absolute value descending", {
  x_tr3 <- as.matrix(mtcars[-(1:5), c("cyl", "disp", "wt")])
  y_tr3 <- mtcars[-(1:5), "mpg"]
  fit3  <- glmnet::glmnet(x_tr3, y_tr3, alpha = 0, family = "gaussian")
  rc3   <- reason_code(fit3, as.matrix(mtcars[1:5, c("cyl", "disp", "wt")]),
                       x_train = x_tr3, s = fit3$lambda[5], n_reasons = 3L)
  for (i in seq_len(nrow(rc3$reasons))) {
    abs_vals <- abs(as.numeric(
      rc3$reasons[i, grep("contribution", names(rc3$reasons))]
    ))
    testthat::expect_true(all(diff(abs_vals) <= 0))
  }
})

testthat::test_that("missing x_train raises an error", {
  testthat::expect_error(
    reason_code(fit_gauss, x_new, s = s_val),
    "x_train"
  )
})

testthat::test_that("missing s raises an error", {
  testthat::expect_error(
    reason_code(fit_gauss, x_new, x_train = x_train),
    "s"
  )
})
