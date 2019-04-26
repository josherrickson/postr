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

#' @rdname postr_classificationtable
#' @export
pr_classificationtable <- postr_classificationtable

#' @rdname postr_classificationtable
#' @export
postr_classtable <- postr_classificationtable

#' @rdname postr_classificationtable
#' @export
pr_classtable <- postr_classificationtable
