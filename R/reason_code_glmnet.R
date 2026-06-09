# Copyright 2022 Robert Carnell

#' Reason codes for a glmnet model
#'
#' @description Computes reason codes for a [`glmnet`][glmnet::glmnet] model.
#'   Because `glmnet` does not store training data, the training predictor
#'   matrix `x_train` must be supplied. Contributions are calculated on the
#'   link scale as `beta_j * (x_j - mean_train_j)` and predictors are ranked
#'   by absolute contribution. Predictions and baseline are on the response
#'   scale.
#'
#' @inheritParams reason_code
#' @param newdata a numeric matrix of predictors for new observations (no
#'   intercept column), with the same columns as `x_train`
#' @param x_train the numeric matrix of predictors used to fit `model` (no
#'   intercept column); required because `glmnet` does not store training data
#' @param s a single numeric value of the penalty parameter `lambda` at which
#'   to extract coefficients and compute predictions; see
#'   [glmnet::predict.glmnet()]
#'
#' @return a [`reason_codes`] object. See [reason_code()] for details.
#'   `contributions` are on the link scale; `predictions` and `baseline` are
#'   on the response scale.
#'
#' @method reason_code glmnet
#' @importFrom stats coef predict
#' @export
#' @seealso [reason_code()]
#'
#' @examples
#' if (requireNamespace("glmnet", quietly = TRUE)) {
#'   x_train <- as.matrix(mtcars[-(1:2), c("cyl", "wt")])
#'   y_train <- mtcars[-(1:2), "mpg"]
#'   x_new   <- as.matrix(mtcars[1:2, c("cyl", "wt")])
#'   fit <- glmnet::glmnet(x_train, y_train, alpha = 1, family = "gaussian")
#'   rc  <- reason_code(fit, x_new, x_train = x_train, s = fit$lambda[10])
#'   print(rc)
#' }
reason_code.glmnet <- function(model, newdata, n_reasons = 3,
                               x_train, s, ...) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required. Install it with install.packages('glmnet').",
         call. = FALSE)
  }
  if (missing(x_train)) {
    stop("'x_train' must be supplied: glmnet does not store training data.",
         call. = FALSE)
  }
  if (missing(s) || length(s) != 1L) {
    stop("'s' must be a single lambda value from model$lambda.", call. = FALSE)
  }
  stopifnot(
    is.matrix(newdata) || is.data.frame(newdata),
    is.matrix(x_train) || is.data.frame(x_train),
    is.numeric(n_reasons), length(n_reasons) == 1L, n_reasons >= 1L
  )
  newdata <- as.matrix(newdata)
  x_train <- as.matrix(x_train)

  # Coefficients: sparse (p+1) x 1 matrix; first row is the intercept
  coef_mat   <- stats::coef(model, s = s)
  coef_vec   <- as.numeric(coef_mat)
  pred_names <- rownames(coef_mat)[-1L]
  betas      <- coef_vec[-1L]
  names(betas) <- pred_names

  if (ncol(newdata) != length(betas)) {
    stop(sprintf(
      "'newdata' has %d columns but model has %d predictors.",
      ncol(newdata), length(betas)
    ), call. = FALSE)
  }

  col_means <- colMeans(x_train)

  deviations        <- sweep(newdata,    2L, col_means, "-")
  contributions_raw <- sweep(deviations, 2L, betas,     "*")
  colnames(contributions_raw) <- pred_names

  n_reasons <- min(as.integer(n_reasons), length(betas))

  # Baseline on response scale (prediction at training column means)
  baseline_x <- matrix(col_means, nrow = 1L,
                       dimnames = list(NULL, colnames(x_train)))
  baseline <- as.numeric(stats::predict(model, newx = baseline_x, s = s,
                                        type = "response"))

  preds <- as.numeric(stats::predict(model, newx = newdata, s = s,
                                     type = "response"))

  contributions_df <- as.data.frame(contributions_raw)

  new_reason_codes(
    predictions   = preds,
    baseline      = baseline,
    contributions = contributions_df,
    reasons       = .build_reason_df(contributions_df, n_reasons),
    model_class   = paste0("glmnet (", .glmnet_family_label(model), ")")
  )
}

.glmnet_family_label <- function(model) {
  lbl <- c(
    elnet     = "gaussian",
    lognet    = "binomial",
    fishnet   = "poisson",
    poissonnet = "poisson",
    multnet   = "multinomial",
    coxnet    = "cox",
    mrelnet   = "mgaussian"
  )[class(model)[1L]]
  if (is.na(lbl)) class(model)[1L] else unname(lbl)
}
