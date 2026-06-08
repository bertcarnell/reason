# Copyright 2022 Robert Carnell

#' Get reason codes for model predictions
#'
#' @description Provides reason codes identifying which covariates most
#'   influenced each prediction. Each covariate's contribution is measured as
#'   `beta_j * (x_j - mean_train_j)` (on the link scale for GLMs), where the
#'   training-data column means serve as the baseline. Covariates are then
#'   ranked by the absolute value of their contribution.
#'
#' @param model a fitted model object
#' @param newdata a `data.frame` of new observations for prediction, in the
#'   same format as the data used to fit the model
#' @param n_reasons integer, the number of top reason codes to return per
#'   observation (default 3)
#' @param ... further arguments passed to model-specific methods
#'
#' @return a `reason_codes` object containing:
#'   \describe{
#'     \item{`predictions`}{numeric vector of predicted values on the response scale}
#'     \item{`baseline`}{numeric scalar, prediction when all predictors equal
#'       their training-data means}
#'     \item{`contributions`}{`data.frame` of per-predictor contributions to the
#'       linear predictor, one row per observation}
#'     \item{`reasons`}{`data.frame` of top `n_reasons` reason codes per
#'       observation, with interleaved name and contribution columns
#'       (`reason_1`, `reason_1_contribution`, `reason_2`, ...)}
#'     \item{`model_class`}{character label for the model type}
#'   }
#'
#' @seealso [reason_code.lm()], [reason_code.glm()]
#'
#' @export
reason_code <- function(model, newdata, n_reasons = 3, ...)
{
  UseMethod("reason_code", model)
}
