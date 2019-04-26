#' postr_classify
#'
#' @param model A glm logistic model
#' @param threshold Some numeric threshold
#'
#' @return a vector of classifications
#' @export
#' @importFrom stats predict
#' @import methods
postr_classify <- function(model, threshold) {
  UseMethod("postr_classify")
}

#' @export
postr_classify.default <- function(model, threshold) {
  stop(paste0("classify not supported for class ", class(model), "."))

}

#' @export
postr_classify.glm <- function(model, threshold) {
  .glm.families.supported(model, "binomial")

  if (threshold < 0 | threshold > 1) {
    stop("Thresholds must be in [0,1]")
  }
  if (!is(model, "glm")) {
    stop("model must be result of `glm`")
  }
  if (model$family$family != "binomial") {
    stop("glm family must be binomial")
  }

  predict(model, type = "response") > threshold
}

#' @rdname postr_classify
#' @export
pr_classify <- postr_classify
