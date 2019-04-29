#' Return leverage values
#'
#' @param model A supported model
#'
#' @return Leverage values
#' @importFrom stats hatvalues
#' @export
postr_leverage <- function(model) {
  UseMethod("postr_leverage")
}

#' @export
postr_leverage.default <- function(model) {
  stop(paste0("Obtaining observed values not supported for class ", class(model), "."))
}

#' @export
postr_leverage.lm <- function(model) {
  hatvalues(model)
}

#' @rdname postr_leverage
#' @export
pr_leverage <- postr_leverage
