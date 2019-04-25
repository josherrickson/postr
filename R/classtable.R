#' Classification Table
#'
#' @param model A glm logistic model
#' @param threshold Some numeric threshold
#' @param ... Additional arguments to `table`.
#'
#' @return A table object
#' @export
postr_classtable <- function(model, threshold, ...) {
  classified <- as.numeric(postr_classify(model, threshold))
  observed <- model$data[,as.character(attributes(model$terms)$variables[[2]])]

  table(Observed = factor(observed, levels = c(0,1)),
        Classified = factor(classified, levels = c(0,1)),
        ...)
}

#' @rdname postr_classtable
#' @export
pr_classtable <- postr_classtable
