#' (Internal) function to check GLM family support
#'
#' @param model A glm object
#' @param ... any number of families, e.g. "binomial", "poisson"
#'
#' @return TRUE if family of `model` is in list, otherwise informative
#'  errors if family of model is not in the list.
.glm.families.supported <- function(model, ...) {
  supported <- as.vector(as.list(match.call())[-(1:2)])
  if (!(model$family$family %in% supported)) {
    if (length(supported) == 1) {
      stop(paste("glm family must be", supported))
    } else {
      stop(paste("glm family must be one of", paste(supported, collapse = ", ")))
    }
  }
  return(TRUE)
}
