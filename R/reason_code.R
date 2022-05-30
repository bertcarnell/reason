# Copyright 2022 Robert Carnell

#' Generic reason_code method
#'
#' @description blah blah blah
#'
#' @param model a model object
#' @param newdata data for prediction of the same format as was originally fit
#' @param ... further arguments for other methods
#'
#' @return a data.frame containing reasons
#'
#' @seealso \code{\link{reason_code.lm}}
#'
#' @export
reason_code <- function(model, newdata, ...)
{
  UseMethod("reason_code", model)
}
