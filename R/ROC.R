#' plot ROC curve
#'
#' @param model A glm logistic model
#' @param thresholds Some numeric threshold
#' @param AUC Logical; should AUC be printed on the plot? Default TRUE.
#'
#' @return ggplot object
#' @export
#' @import ggplot2
postr_ROC <- function(model,
                      thresholds = seq(0, 1, by = .01),
                      AUC = TRUE) {
  thresholds <- sort(unique(thresholds))
  if (min(thresholds) == 0) {
    thresholds <- thresholds[-1]
  }
  if (max(thresholds) == 1) {
    thresholds <- thresholds[-length(thresholds)]
  }
  tpr <- c(1, postr_tpr(model, thresholds), 0)
  fpr <- c(1, postr_fpr(model, thresholds), 0)

  # reverse all vectors so ggplot connects points
  # appropriately
  d <- data.frame(thresholds = c(1, rev(thresholds), 0),
                  tpr = rev(tpr),
                  fpr = rev(fpr))
  d <- d[!duplicated(d[,-1]),] # Duplicate TNR/TPR slow down plotting

  if (AUC) {
    auc <- round(.AUC(d$thresholds, rev(d$tpr), rev(d$fpr)), 3)
  }

  g <- ggplot(d, aes(x = fpr, y = tpr)) +
    geom_point() +
    geom_line() +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey") +
    xlab("False Positive Rate") +
    ylab("True Positive Rate")
  if (AUC) {
    g <- g +
      geom_label(aes(x = 1, y = .1, label = paste("AUC =", auc)),
                 hjust = "inward")
  }
  g
}

#' @rdname postr_ROC
#' @export
pr_ROC <- postr_ROC