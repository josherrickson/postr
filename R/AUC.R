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

#' @export
postr_AUC.glm <- function(model, thresholds = seq(0, 1, by = .01)) {
  .glm.families.supported(model, "binomial")

  thresholds <- sort(unique(thresholds))
  if (min(thresholds) == 0) {
    thresholds <- thresholds[-1]
  }
  if (max(thresholds) == 1) {
    thresholds <- thresholds[-length(thresholds)]
  }
  tpr <- c(1, postr_tpr(model, thresholds), 0)
  fpr <- c(1, postr_fpr(model, thresholds), 0)

  tpr <- tpr[!duplicated(fpr)]
  fpr <- fpr[!duplicated(fpr)] # Duplicate TNR/TPR slow down plotting

  .AUC(tpr, fpr)
}

#' @rdname postr_AUC
#' @export
pr_AUC <- postr_AUC

.AUC <- function(tpr, fpr) {
  sum(-diff(fpr)*(tpr[-1] + tpr[-length(tpr)])/2)
}
