# Copyright 2022 Robert Carnell

#' Create a reason_codes object
#'
#' @param predictions numeric vector of predicted values on the response scale
#' @param baseline numeric scalar, prediction when all predictors equal their
#'   training-data means
#' @param contributions `data.frame` of per-predictor contributions to the
#'   linear predictor, one row per observation, one column per predictor
#' @param reasons `data.frame` of top reason codes with interleaved name and
#'   value columns
#' @param model_class character label for the model type
#'
#' @return an object of class `reason_codes`
#'
#' @keywords internal
new_reason_codes <- function(predictions, baseline, contributions, reasons,
                             model_class) {
  structure(
    list(
      predictions  = predictions,
      baseline     = baseline,
      contributions = contributions,
      reasons      = reasons,
      model_class  = model_class
    ),
    class = "reason_codes"
  )
}

#' Print a reason_codes object
#'
#' @param x a `reason_codes` object
#' @param digits integer, number of significant digits to display (default 4)
#' @param ... further arguments (ignored)
#'
#' @return invisibly returns `x`
#'
#' @method print reason_codes
#' @export
print.reason_codes <- function(x, digits = 4, ...) {
  cat("Reason Codes for", x$model_class, "model\n")
  cat("Baseline prediction:", round(x$baseline, digits), "\n\n")
  out <- cbind(
    data.frame(prediction = round(unname(x$predictions), digits)),
    x$reasons
  )
  print(out, row.names = TRUE)
  invisible(x)
}

#' Convert a reason_codes object to a data.frame
#'
#' @param x a `reason_codes` object
#' @param ... further arguments (ignored)
#'
#' @return a `data.frame` with a `prediction` column followed by the reason
#'   code columns
#'
#' @method as.data.frame reason_codes
#' @export
as.data.frame.reason_codes <- function(x, ...) {
  cbind(data.frame(prediction = unname(x$predictions)), x$reasons)
}

#' Summarise a reason_codes object
#'
#' @param object a `reason_codes` object
#' @param ... further arguments (ignored)
#'
#' @return invisibly returns `object`
#'
#' @method summary reason_codes
#' @export
summary.reason_codes <- function(object, ...) {
  cat("Reason Codes Summary\n")
  cat("  Model class        :", object$model_class, "\n")
  cat("  Observations       :", nrow(object$contributions), "\n")
  cat("  Predictors         :", ncol(object$contributions), "\n")
  cat("  Baseline prediction:", round(object$baseline, 4), "\n\n")

  cat("Prediction summary:\n")
  print(summary(unname(object$predictions)))

  reason_cols <- grep("^reason_[0-9]+$", names(object$reasons), value = TRUE)
  if (length(reason_cols) > 0) {
    cat("\nTop-reason frequencies:\n")
    for (col in reason_cols) {
      cat("\n  ", col, ":\n", sep = "")
      tbl <- sort(table(object$reasons[[col]]), decreasing = TRUE)
      print(tbl)
    }
  }
  invisible(object)
}
