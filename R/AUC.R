#' Calculate AUC
#'
#' @param model A glm logistic model
#' @param thresholds Some numeric thresholds
#'
#' @return AUC
#' @export
postr_AUC <- function(model, thresholds = seq(0, 1, by = .01)) {
  UseMethod("postr_AUC")
}

#' @export
postr_AUC.default <- function(model, thresholds = seq(0, 1, by = .01)) {
  stop(paste0("AUC not supported for class ", class(model), "."))
}

#' @rdname postr_AUC
#' @export
pr_AUC <- postr_AUC
