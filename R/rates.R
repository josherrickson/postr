#' Classification Rates
#'
#' Returns a classification rate.
#'
#' @details `postr_sensitivity` and `postr_specificity` are aliases `postr_tpr` and `postr_tnr`.
#'
#' @param model A glm logistic model
#' @param threshold Some numeric thresholds
#'
#' @return A rate
#' @export
#' @import methods
#' @rdname postr_rates
postr_tpr <- function(model, threshold) {
  getrate(model, threshold, obs = 1, class =  1)
}

#' @export
#' @rdname postr_rates
postr_sensitivity <- postr_tpr

#' @export
#' @rdname postr_rates
pr_tpr <- postr_tpr

#' @export
#' @rdname postr_rates
pr_sensitivity <- postr_tpr

#' @export
#' @rdname postr_rates
postr_tnr <- function(model, threshold) {
  getrate(model, threshold, obs = 0, class =  0)
}

#' @export
#' @rdname postr_rates
postr_specificity <- postr_tnr

#' @export
#' @rdname postr_rates
pr_tnr <- postr_tnr

#' @export
#' @rdname postr_rates
pr_specificity <- postr_tnr

#' @export
#' @rdname postr_rates
postr_fpr <- function(model, threshold) {
  getrate(model, threshold, obs = 0, class =  1)
}

#' @export
#' @rdname postr_rates
pr_fpr <- postr_fpr

#' @export
#' @rdname postr_rates
postr_fnr <- function(model, threshold) {
  getrate(model, threshold, obs = 1, class =  0)
}

#' @export
#' @rdname postr_rates
pr_fnr <- postr_fnr
