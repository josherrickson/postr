#' Return observed values
#'
#' @param model A support model
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

#' @rdname postr_observed
#' @export
pr_observed <- postr_observed
