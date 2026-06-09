# Copyright 2022 Robert Carnell

#' Reason codes for a linear model
#'
#' @description Computes reason codes for an [`lm`][stats::lm] object.
#'   The contribution of predictor `j` for observation `i` is
#'   `beta_j * (x_ij - mean_train_j)`, where `mean_train_j` is the column
#'   mean from the training design matrix. Contributions for all predictors
#'   sum to `prediction_i - baseline`. Predictors are ranked by absolute
#'   contribution.
#'
#' @inheritParams reason_code
#'
#' @return a [`reason_codes`] object. See [reason_code()] for the full
#'   description of its components.
#'
#' @method reason_code lm
#' @importFrom stats coef delete.response model.matrix predict.lm terms
#' @export
#' @seealso [reason_code()]
#'
#' @examples
#' lm1 <- lm(mpg ~ cyl + wt, data = mtcars[-(1:2), ])
#' rc1 <- reason_code(lm1, mtcars[1:2, ])
#' print(rc1)
#' as.data.frame(rc1)
reason_code.lm <- function(model, newdata, n_reasons = 3, ...) {
  stopifnot(
    is.data.frame(newdata) || is.matrix(newdata),
    is.numeric(n_reasons), length(n_reasons) == 1L, n_reasons >= 1L
  )

  coefs    <- stats::coef(model)
  train_mm <- stats::model.matrix(model)           # design matrix from training
  col_means <- colMeans(train_mm)

  new_mm <- stats::model.matrix(
    stats::delete.response(stats::terms(model)),
    data = newdata
  )

  # contributions on the response scale (lm is linear)
  deviations       <- sweep(new_mm,       2L, col_means, "-")
  contributions_raw <- sweep(deviations,  2L, coefs,     "*")

  # drop the intercept column (deviation is always 0)
  keep <- colnames(contributions_raw) != "(Intercept)"
  contributions_raw <- contributions_raw[, keep, drop = FALSE]

  n_reasons <- min(as.integer(n_reasons), ncol(contributions_raw))

  baseline <- as.numeric(col_means %*% coefs)
  preds    <- stats::predict.lm(model, newdata = newdata)

  new_reason_codes(
    predictions   = preds,
    baseline      = baseline,
    contributions = as.data.frame(contributions_raw),
    reasons       = .build_reason_df(as.data.frame(contributions_raw), n_reasons),
    model_class   = "lm"
  )
}
