# Copyright 2022 Robert Carnell

#' Reason codes for a Cox proportional hazards model
#'
#' @description Computes reason codes for a [`coxph`][survival::coxph] object.
#'   The contribution of predictor `j` for observation `i` is
#'   `beta_j * (x_ij - mean_train_j)`, where `mean_train_j` is the centering
#'   value stored in the fitted model (`model$means`). Contributions are on the
#'   log-hazard-ratio scale; predictions are on the hazard-ratio (risk) scale.
#'   The baseline corresponds to all predictors at their training means, giving
#'   a hazard ratio of 1.
#'
#' @inheritParams reason_code
#'
#' @return a [`reason_codes`] object. See [reason_code()] for details.
#'   `contributions` are on the log-hazard-ratio scale; `predictions` are on
#'   the hazard-ratio scale; `baseline` is always 1.
#'
#' @method reason_code coxph
#' @importFrom stats coef delete.response model.frame model.matrix predict terms
#' @export
#' @seealso [reason_code()]
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   data(veteran, package = "survival")
#'   fit <- survival::coxph(survival::Surv(time, status) ~ age + karno,
#'                          data = veteran[-(1:5), ])
#'   rc  <- reason_code(fit, veteran[1:5, ])
#'   print(rc)
#' }
reason_code.coxph <- function(model, newdata, n_reasons = 3, ...) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required. Install it with install.packages('survival').",
         call. = FALSE)
  }
  stopifnot(
    is.data.frame(newdata) || is.matrix(newdata),
    is.numeric(n_reasons), length(n_reasons) == 1L, n_reasons >= 1L
  )

  coefs     <- stats::coef(model)          # no intercept in Cox models
  col_means <- model$means[names(coefs)]   # centering values, aligned to coefs

  # Design matrix for new data (no intercept expected for coxph)
  trms   <- stats::delete.response(stats::terms(model))
  new_mf <- stats::model.frame(trms, data = newdata, xlev = model$xlevels)
  new_mm <- stats::model.matrix(trms, data = new_mf)

  # Remove intercept column if unexpectedly present
  new_mm <- new_mm[, colnames(new_mm) != "(Intercept)", drop = FALSE]

  # Align to coefficient order
  new_mm <- new_mm[, names(coefs), drop = FALSE]

  # Contributions on the log-hazard-ratio scale
  deviations        <- sweep(new_mm,       2L, col_means, "-")
  contributions_raw <- sweep(deviations,   2L, coefs,     "*")

  n_reasons <- min(as.integer(n_reasons), ncol(contributions_raw))

  # When all predictors are at training means, log HR = 0 => HR = 1
  baseline <- 1.0

  preds <- stats::predict(model, newdata = newdata, type = "risk")

  contributions_df <- as.data.frame(contributions_raw)

  new_reason_codes(
    predictions   = preds,
    baseline      = baseline,
    contributions = contributions_df,
    reasons       = .build_reason_df(contributions_df, n_reasons),
    model_class   = "coxph"
  )
}
