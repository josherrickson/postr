#' Classification Rates
#'
#' Returns a classification rate.
#'
#' @details `gp_sensitivity` and `gp_specificity` are aliases `gp_tpr` and `gp_tnr`.
#'
#' @param model A glm logistic model
#' @param threshold Some numeric threshold
#'
#' @return A rate
#' @export
#' @import methods
#' @rdname gp_rates
gp_tpr <- function(model, threshold) {
  getrate(model, threshold, obs = 1, class =  1)
}

#' @export
#' @rdname gp_rates
gp_sensitivity <- gp_tpr

#' @export
#' @rdname gp_rates
gp_tnr <- function(model, threshold) {
  getrate(model, threshold, obs = 0, class =  0)
}

#' @export
#' @rdname gp_rates
gp_specificity <- gp_tnr

#' @export
#' @rdname gp_rates
gp_fpr <- function(model, threshold) {
  getrate(model, threshold, obs = 0, class =  1)
}

#' @export
#' @rdname gp_rates
gp_fnr <- function(model, threshold) {
  getrate(model, threshold, obs = 1, class =  0)
}
