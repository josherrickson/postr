#' Classification Table
#'
#' @param model A glm logistic model
#' @param threshold Some numeric threshold
#' @param ... Additional arguments to `table`.
#'
#' @return A table object
#' @export
postr_classificationtable <- function(model, threshold, ...) {
  UseMethod("postr_classificationtable")
}

#' @export
postr_classificationtable.default <- function(model, threshold, ...) {
  stop(paste0("Classification table not supported for class ", class(model), "."))
}

#' @export
postr_classificationtable.glm <- function(model, threshold, ...) {
  .glm.families.supported(model, "binomial")

  classified <- as.numeric(postr_classify(model, threshold))
  observed <- model$data[,as.character(attributes(model$terms)$variables[[2]])]

  table(Observed = factor(observed, levels = c(0,1)),
        Classified = factor(classified, levels = c(0,1)),
        ...)
}

#' @rdname postr_classificationtable
#' @export
pr_classificationtable <- postr_classificationtable

#' @rdname postr_classificationtable
#' @export
postr_classtable <- postr_classificationtable

#' @rdname postr_classificationtable
#' @export
pr_classtable <- postr_classificationtable
