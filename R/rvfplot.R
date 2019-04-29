#' Residual vs Fitted plot
#'
#' @param model A model
#' @param smoothedcurve Logical; should a smoothed curve be plotted? Default: False
#'
#' @return A ggplot object
#' @importFrom stats residuals fitted
#' @export
postr_rvfplot <- function(model, smoothedcurve = FALSE) {
  UseMethod("postr_rvfplot")
}

#' @export
postr_rvfplot.default <- function(model, smoothedcurve) {
  stop(paste0("RVF plot not supported for class ", class(model), "."))
}

#' @export
postr_rvfplot.lm <- function(model, smoothedcurve = FALSE) {
  res <- residuals(model)
  fit <- fitted(model)
  d <- data.frame(res = res, fit = fit)
  g <- ggplot(d, aes(x = fit, y = res)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    xlab("Fitted Values") +
    ylab("Residuals")
  if (smoothedcurve) {
    g <- g + geom_smooth(method = "loess")
  }
  g
}

#' @export
postr_rvfplot.glm <- postr_rvfplot.lm

#' @rdname postr_rvfplot
#' @export
pr_rvfplot <- postr_rvfplot
