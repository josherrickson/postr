#' Return leverage values
#'
#' @param model A supported model
#' @param force Force calculation in non-supported models.
#'
#' @return Leverage values
#' @importFrom stats hatvalues
#' @export
postr_leverage <- function(model, force = FALSE) {
  UseMethod("postr_leverage")
}

#' @export
postr_leverage.default <- function(model, ...) {
  stop(paste0("Obtaining leverage values not supported for class ", class(model), "."))
}

#' @export
postr_leverage.lm <- function(model, ...) {
  hatvalues(model)
}

#' @export
postr_leverage.glm <- postr_leverage.lm

#' @export
postr_leverage.lmerMod <- postr_leverage.lm

#' @export
postr_leverage.glmerMod <- function(model, force = FALSE) {
  if (!force) {
    stop("Leverage may not make sense for GLMM models.\n use argument 'force=TRUE' to bypass and compute.")
  }
  suppressWarnings(hatvalues(model))
}


#' @rdname postr_leverage
#' @export
pr_leverage <- postr_leverage
