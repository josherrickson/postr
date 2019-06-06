#' postr_classify
#'
#' Given a model and some threshold, estimate the classification for each observation.
#'
#' @param model A supported model.
#' @param threshold A numeric value in the range [0,1].
#' @param newdata An optional dataframe of observations to classify instead of the observations used in building the model.
#'
#' @return A vector of logical classifications.
#' @export
#' @importFrom stats predict
#' @import methods
postr_classify <- function(model, threshold, newdata = NULL) {
  UseMethod("postr_classify")
}

#' @export
postr_classify.default <- function(model, threshold, newdata = NULL) {
  stop(paste0("classify not supported for class ", class(model), "."))

}

#' @export
postr_classify.glm <- function(model, threshold, newdata = NULL) {
  .glm.families.supported(model, "binomial")

  if (threshold < 0 | threshold > 1) {
    stop("Thresholds must be in [0,1]")
  }

  if (is.null(newdata)) {
    predict(model, type = "response") > threshold
  } else {
    predict(model, type = "response", newdata = newdata) > threshold
  }
}

#' @export
postr_classify.glmerMod <- postr_classify.glm

#' @rdname postr_classify
#' @export
pr_classify <- postr_classify
