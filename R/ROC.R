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
  UseMethod("postr_ROC")
}

#' @export
postr_ROC.default <- function(model,
                              thresholds = seq(0, 1, by = .01),
                              AUC = TRUE) {
  stop(paste0("ROC not supported for class ", class(model), "."))

}

#' @rdname postr_ROC
#' @export
pr_ROC <- postr_ROC
