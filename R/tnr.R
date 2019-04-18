#' gp_tnr
#'
#' @param model A glm logistic model
#' @param threshold Some numeric threshold
#'
#' @return The true negative rate
#' @export
#' @import methods
gp_tnr <- function(model, threshold) {
  stopifnot(is(model, "glm"))
  stopifnot(model$family$family == "binomial")

  classification <- glmpost::gp_classify(model, threshold)
  observed <- model$data[,as.character(attributes(model$terms)$variables[[2]])]
  tn <- sum(observed == 0)
  pn <- sum(classification == 0 & observed == 0)
  pn/tn
}

#' @export
#' @rdname gp_tnr
gp_specificity <- gp_tnr
