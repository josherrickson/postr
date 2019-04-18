#' gp_tpr
#'
#' @param model A glm logistic model
#' @param threshold Some numeric threshold
#'
#' @return The true positive rate
#' @export
#' @import methods
gp_tpr <- function(model, threshold) {
  stopifnot(is(model, "glm"))
  stopifnot(model$family$family == "binomial")

  classification <- glmpost::gp_classify(model, threshold)
  observed <- model$data[,as.character(attributes(model$terms)$variables[[2]])]
  tp <- sum(observed == 1)
  pp <- sum(classification == 1 & observed == 1)
  pp/tp
}

#' @export
#' @rdname gp_tpr
gp_sensitivity <- gp_tpr
