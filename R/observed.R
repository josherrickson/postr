#' Return observed values
#'
#' @param model A supported model
#'
#' @return Observed values
#' @export
postr_observed <- function(model) {
  UseMethod("postr_observed")
}

#' @export
postr_observed.default <- function(model) {
  stop(paste0("Obtaining observed values not supported for class ", class(model), "."))
}

#' @export
postr_observed.glm <- function(model) {
  model$data[,as.character(attributes(model$terms)$variables[[2]])]
}

#' @export
postr_observed.glmerMod <- function(model) {
  model@frame[,1]
}

#' @rdname postr_observed
#' @export
pr_observed <- postr_observed
