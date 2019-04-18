#' Classification Table
#'
#' @param model A glm logistic model
#' @param threshold Some numeric threshold
#' @param ... Additional arguments to `table`.
#'
#' @return A table object
#' @export
gp_classtable <- function(model, threshold, ...) {
  stopifnot(is(model, "glm"))
  stopifnot(model$family$family == "binomial")

  observed <- model$data[,as.character(attributes(model$terms)$variables[[2]])]
  classified <- as.numeric(gp_classify(model, threshold))

  table(Observed = factor(observed, levels = c(0,1)),
        Classified = factor(classified, levels = c(0,1)),
        ...)
}
