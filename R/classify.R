#' gp_classify
#'
#' @param model A glm logistic model
#' @param threshold Some numeric threshold
#'
#' @return a vector of classifications
#' @export
gp_classify <- function(model, threshold) {
  stopifnot(is(model, "glm"))
  stopifnot(model$family$family == "binomial")

  predict(model) > threshold
}
