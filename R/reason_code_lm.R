#' Reason Code for a linear model
#'
#' @inherit reason_code description
#'
#' @inheritParams reason_code
#'
#' @inherit reason_code return
#'
#' @method reason_code lm
#' @importFrom stats lm predict
#' @export
#' @seealso \code{\link{reason_code}}
#'
#' @examples
#' lm1 <- lm(mpg ~ cyl + wt, data = mtcars[-(1:2),])
#' reason_code(lm1, mtcars[1:2,])
reason_code.lm <- function(model, newdata, ...)
{
  print("Do something with the model")
  preds <- stats::predict.lm(model, newdata = newdata)
  return(preds)
}
