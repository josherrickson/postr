#' Calculate AUC
#'
#' @param model A glm logistic model
#' @param thresholds Some numeric thresholds
#'
#' @return AUC
#' @export
postr_AUC <- function(model, thresholds = seq(0, 1, by = .01)) {
  thresholds <- sort(unique(thresholds))
  if (min(thresholds) == 0) {
    thresholds <- thresholds[-1]
  }
  if (max(thresholds) == 1) {
    thresholds <- thresholds[-length(thresholds)]
  }
  tpr <- c(1, postr_tpr(model, thresholds), 0)
  fpr <- c(1, postr_fpr(model, thresholds), 0)

  sum(diff(c(0, thresholds, 1))*(tpr[-1] + fpr[-1])/2)
}

#' @rdname postr_AUC
#' @export
pr_AUC <- postr_AUC
