#' plot Sensitivity (TPR) and Specificity (TNR) curves
#'
#' @param model A glm logistic model
#' @param tpr.color Color for sensitivity (TPR) line
#' @param tnr.color Color for specificity (TNR) line
#'
#' @return ggplot object
#' @export
#' @import ggplot2
postr_sensplot <- function(model,
                           tpr.color = "red",
                           tnr.color = "blue") {
  UseMethod("postr_sensplot")
}

#' @export
postr_sensplot.default <- function(model,
                                   tpr.color = "red",
                                   tnr.color = "blue") {
  stop(paste0("Sensitivity plot not supported for class ", class(model), "."))

}

#' @export
postr_sensplot.glm <- function(model,
                               tpr.color = "red",
                               tnr.color = "blue") {
  .glm.families.supported(model, "binomial")

  pred <- sort(unique(c(0, 1, predict(model, type = "response"))))

  tpr <- postr_tpr(model, pred)
  tnr <- postr_tnr(model, pred)

  d <- data.frame(pred = pred,
                  tpr = tpr,
                  tnr = tnr)
  d <- d[-duplicated(d[,2:3]),] # Duplicate TNR/TPR slow down plotting

  g <- ggplot(d, aes(x = pred, y = tpr)) +
    geom_point(color = tpr.color) +
    geom_line(color = tpr.color) +
    geom_point(aes(y = tnr), color = tnr.color) +
    geom_line(aes(y = tnr), color = tnr.color) +
    xlab("Threshold") +
    ylab("TPR/TNR")

  return(g)
}

#' @export
postr_sensplot.glmerMod <- postr_sensplot.glm

#' @rdname postr_sensplot
#' @export
pr_sensplot <- postr_sensplot
