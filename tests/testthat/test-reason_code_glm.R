glm1 <- glm(am ~ cyl + wt, data = mtcars[-(1:2), ], family = binomial)
rc1  <- reason_code(glm1, mtcars[1:2, ])

testthat::test_that("reason_code.glm returns a reason_codes object", {
  testthat::expect_s3_class(rc1, "reason_codes")
})

testthat::test_that("predictions are on the response (probability) scale", {
  testthat::expect_true(all(rc1$predictions >= 0 & rc1$predictions <= 1))
})

testthat::test_that("baseline is on the response scale", {
  testthat::expect_true(rc1$baseline >= 0 && rc1$baseline <= 1)
})

testthat::test_that("predictions has one value per new observation", {
  testthat::expect_length(rc1$predictions, 2L)
})

testthat::test_that("contributions has correct dimensions", {
  testthat::expect_equal(nrow(rc1$contributions), 2L)
  testthat::expect_equal(ncol(rc1$contributions), 2L)
  testthat::expect_named(rc1$contributions, c("cyl", "wt"))
})

testthat::test_that("model_class includes family and link", {
  testthat::expect_match(rc1$model_class, "glm")
  testthat::expect_match(rc1$model_class, "binomial")
  testthat::expect_match(rc1$model_class, "logit")
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
  testthat::expect_equal(nrow(df), 2L)
})

testthat::test_that("print.reason_codes runs without error for glm", {
  testthat::expect_output(print(rc1), "Reason Codes for glm")
})

testthat::test_that("summary.reason_codes runs without error for glm", {
  testthat::expect_output(summary(rc1), "Reason Codes Summary")
})

testthat::test_that("Poisson glm works", {
  glm_pois <- glm(carb ~ cyl + wt, data = mtcars[-(1:2), ], family = poisson)
  rc_pois  <- reason_code(glm_pois, mtcars[1:2, ])
  testthat::expect_s3_class(rc_pois, "reason_codes")
  testthat::expect_match(rc_pois$model_class, "poisson")
  # Predictions should be positive counts
  testthat::expect_true(all(rc_pois$predictions > 0))
})

testthat::test_that("single new observation works for glm", {
  rc_single <- reason_code(glm1, mtcars[1, ])
  testthat::expect_length(rc_single$predictions, 1L)
  testthat::expect_equal(nrow(rc_single$contributions), 1L)
})

testthat::test_that("glm dispatches before lm for glm objects", {
  # glm objects have class c("glm","lm"); should use reason_code.glm
  testthat::expect_match(rc1$model_class, "glm")
})

testthat::test_that("contributions are ranked by absolute value descending", {
  glm2 <- glm(am ~ cyl + disp + wt, data = mtcars[-(1:5), ],
               family = binomial)
  rc2 <- reason_code(glm2, mtcars[1:5, ], n_reasons = 3L)
  for (i in seq_len(nrow(rc2$reasons))) {
    abs_vals <- abs(as.numeric(
      rc2$reasons[i, grep("contribution", names(rc2$reasons))]
    ))
    testthat::expect_true(all(diff(abs_vals) <= 0))
  }
})
