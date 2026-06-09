# Copyright 2022 Robert Carnell

#' Reason codes for a parametric survival (AFT) model
#'
#' @description Computes reason codes for a [`survreg`][survival::survreg]
#'   object (accelerated failure time model). The contribution of predictor
#'   `j` for observation `i` is `beta_j * (x_ij - mean_train_j)`, computed
#'   on the linear predictor (log-time for log-link distributions) scale.
#'   Predictions are returned on the response (time) scale.
#'
#'   Training data must be accessible either via `model$model` (set
#'   `model = TRUE` when calling [survival::survreg()]) or through the
#'   `train_data` argument.
#'
#' @inheritParams reason_code
#' @param train_data optional `data.frame` of training data used to fit
#'   `model`; required if the model was not fitted with `model = TRUE`
#'
#' @return a [`reason_codes`] object. See [reason_code()] for details.
#'   `contributions` are on the linear predictor (log-time) scale;
#'   `predictions` and `baseline` are on the response (time) scale.
#'
#' @method reason_code survreg
#' @importFrom stats coef delete.response model.frame model.matrix predict terms
#' @export
#' @seealso [reason_code()]
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   data(veteran, package = "survival")
#'   fit <- survival::survreg(survival::Surv(time, status) ~ age + karno,
#'                            data = veteran[-(1:5), ], model = TRUE)
#'   rc  <- reason_code(fit, veteran[1:5, ])
#'   print(rc)
#' }
reason_code.survreg <- function(model, newdata, n_reasons = 3,
                                train_data = NULL, ...) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required. Install it with install.packages('survival').",
         call. = FALSE)
  }
  stopifnot(
    is.data.frame(newdata) || is.matrix(newdata),
    is.numeric(n_reasons), length(n_reasons) == 1L, n_reasons >= 1L
  )

  coefs <- stats::coef(model)   # includes intercept
  trms  <- stats::terms(model)

  # Recover training design matrix from stored model frame or train_data
  trms_no_int <- stats::delete.response(trms)

  if (!is.null(model$model)) {
    train_mf <- model$model
    train_mm <- stats::model.matrix(trms_no_int, data = train_mf)
  } else if (!is.null(train_data)) {
    train_mf <- stats::model.frame(trms_no_int, data = train_data, xlev = model$xlevels)
    train_mm <- stats::model.matrix(trms_no_int, data = train_mf)
  } else {
    stop(
      "Training data is unavailable. Either refit with survreg(..., model = TRUE) ",
      "or supply the 'train_data' argument.",
      call. = FALSE
    )
  }

  col_means <- colMeans(train_mm)

  # Design matrix for new data (reuse trms_no_int computed above)
  new_mf <- stats::model.frame(trms_no_int, data = newdata, xlev = model$xlevels)
  new_mm <- stats::model.matrix(trms_no_int, data = new_mf)

  # Contributions on the linear predictor scale; drop intercept (deviation = 0)
  deviations        <- sweep(new_mm,     2L, col_means[colnames(new_mm)], "-")
  contributions_raw <- sweep(deviations, 2L, coefs[colnames(new_mm)],     "*")
  contributions_raw <- contributions_raw[,
    colnames(contributions_raw) != "(Intercept)", drop = FALSE]

  n_reasons <- min(as.integer(n_reasons), ncol(contributions_raw))

  # Baseline on response scale using the distribution's inverse link
  baseline_lp <- as.numeric(col_means %*% coefs[colnames(train_mm)])
  baseline    <- .survreg_linkinv(model$dist, baseline_lp)

  preds <- as.numeric(stats::predict(model, newdata = newdata, type = "response"))

  contributions_df <- as.data.frame(contributions_raw)

  dist_lbl <- if (!is.null(model$dist)) model$dist else "unknown"

  new_reason_codes(
    predictions   = preds,
    baseline      = baseline,
    contributions = contributions_df,
    reasons       = .build_reason_df(contributions_df, n_reasons),
    model_class   = paste0("survreg (", dist_lbl, ")")
  )
}

# Apply the inverse link function for a survreg distribution.
# Most AFT distributions use a log link (log-time -> time); gaussian and
# logistic use an identity link.
.survreg_linkinv <- function(dist, lp) {
  log_link <- c("weibull", "exponential", "lognormal", "loglogistic",
                "rayleigh", "loggaussian")
  if (!is.null(dist) && dist %in% log_link) exp(lp) else lp
}
