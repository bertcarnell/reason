# Copyright 2022 Robert Carnell

#' Reason codes for a generalized linear model
#'
#' @description Computes reason codes for a [`glm`][stats::glm] object.
#'   Contributions are calculated on the **link** (linear predictor) scale as
#'   `beta_j * (x_ij - mean_train_j)`, where `mean_train_j` is the column mean
#'   of the training design matrix. Predictions are returned on the
#'   **response** scale. Predictors are ranked by absolute contribution on
#'   the link scale.
#'
#' @inheritParams reason_code
#'
#' @return a [`reason_codes`] object. See [reason_code()] for the full
#'   description of its components. Note that `baseline` and `predictions`
#'   are on the response scale while `contributions` are on the link scale.
#'
#' @method reason_code glm
#' @importFrom stats coef delete.response family model.matrix predict.glm terms
#' @export
#' @seealso [reason_code()]
#'
#' @examples
#' glm1 <- glm(am ~ cyl + wt, data = mtcars[-(1:2), ], family = binomial)
#' rc1 <- reason_code(glm1, mtcars[1:2, ])
#' print(rc1)
#' as.data.frame(rc1)
reason_code.glm <- function(model, newdata, n_reasons = 3, ...) {
  stopifnot(
    is.data.frame(newdata) || is.matrix(newdata),
    is.numeric(n_reasons), length(n_reasons) == 1L, n_reasons >= 1L
  )

  coefs     <- stats::coef(model)
  train_mm  <- stats::model.matrix(model)
  col_means <- colMeans(train_mm)

  new_mm <- stats::model.matrix(
    stats::delete.response(stats::terms(model)),
    data = newdata
  )

  # contributions on the link scale
  deviations        <- sweep(new_mm,          2L, col_means, "-")
  contributions_raw <- sweep(deviations,       2L, coefs,    "*")

  keep <- colnames(contributions_raw) != "(Intercept)"
  contributions_raw <- contributions_raw[, keep, drop = FALSE]

  n_reasons <- min(as.integer(n_reasons), ncol(contributions_raw))

  fam <- stats::family(model)

  baseline_link <- as.numeric(col_means %*% coefs)
  baseline      <- fam$linkinv(baseline_link)

  preds <- stats::predict.glm(model, newdata = newdata, type = "response")

  new_reason_codes(
    predictions   = preds,
    baseline      = baseline,
    contributions = as.data.frame(contributions_raw),
    reasons       = .build_reason_df(as.data.frame(contributions_raw), n_reasons),
    model_class   = paste0("glm (", fam$family, "/", fam$link, ")")
  )
}
