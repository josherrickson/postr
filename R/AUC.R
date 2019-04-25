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

  d <- data.frame(thresholds = c(0, thresholds, 1),
                  tpr = tpr,
                  fpr = fpr)
  d <- d[!duplicated(d[,-1]),] # Duplicate TNR/TPR slow down plotting

  .AUC(d$thresholds, d$tpr, d$fpr)
}

#' @rdname postr_AUC
#' @export
pr_AUC <- postr_AUC

.AUC <- function(thresholds, tpr, fpr) {
  sum(diff(thresholds)*(tpr[-1] + fpr[-1])/2)
}
