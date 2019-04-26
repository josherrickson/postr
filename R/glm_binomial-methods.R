#' @export
postr_classify.glm <- function(model, threshold) {
  .glm.families.supported(model, "binomial")

  if (threshold < 0 | threshold > 1) {
    stop("Thresholds must be in [0,1]")
  }

  predict(model, type = "response") > threshold
}

#' @export
postr_classificationtable.glm <- function(model, threshold, ...) {
  .glm.families.supported(model, "binomial")

  classified <- as.numeric(postr_classify(model, threshold))
  observed <- postr_observed(model)

  table(Observed = factor(observed, levels = c(0,1)),
        Classified = factor(classified, levels = c(0,1)),
        ...)
}

#' @export
postr_observed.glm <- function(model) {
  model$data[,as.character(attributes(model$terms)$variables[[2]])]
}

#' @export
postr_tpr.glm <- function(model, threshold) {
  .glm.families.supported(model, "binomial")
  getrate(model, threshold, obs = 1, class =  1)
}

#' @export
postr_tnr.glm <- function(model, threshold) {
  .glm.families.supported(model, "binomial")
  getrate(model, threshold, obs = 0, class =  0)
}

#' @export
postr_fpr.glm <- function(model, threshold) {
  .glm.families.supported(model, "binomial")
  getrate(model, threshold, obs = 0, class =  1)
}

#' @export
postr_fnr.glm <- function(model, threshold) {
  .glm.families.supported(model, "binomial")
  getrate(model, threshold, obs = 1, class =  0)
}

#' @export
postr_ROC.glm <- function(model,
                          thresholds = seq(0, 1, by = .01),
                          AUC = TRUE) {
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

  d <- data.frame(tpr = tpr,
                  fpr = fpr)
  d <- unique(d) # Duplicate TNR/TPR slow down plotting

  if (AUC) {
    dtmp <- d[!duplicated(d$fpr),]
    auc <- round(postr_AUC(model, thresholds), 3)
  }

  # reverse all vectors so ggplot connects points
  # appropriately
  d <- d[nrow(d):1,]

  g <- ggplot(d, aes(x = fpr, y = tpr)) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey") +
    geom_point() +
    geom_line() +
    xlab("False Positive Rate") +
    ylab("True Positive Rate")


  if (AUC) {
    g <- g +
      geom_label(aes(x = 1, y = .1, label = paste("AUC =", auc)),
                 hjust = "inward")
  }
  return(g)
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

  sum(-diff(fpr)*(tpr[-1] + tpr[-length(tpr)])/2)
}
