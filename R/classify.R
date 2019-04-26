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

#' @rdname postr_classify
#' @export
pr_classify <- postr_classify
