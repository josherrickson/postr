#' Compute VIF
#'
#' @param model A supported model
#'
#' @return VIFs
#' @export
#' @importFrom car vif
postr_vif <- function(model) {
  UseMethod("postr_vif")
}

#' @export
postr_vif.default <- function(model) {
  stop(paste0("VIF not supported for class ", class(model), "."))
}

postr_vif.lm <- function(model) {
  car::vif(model)
}

postr_vif.glm <- postr_vif.lm
postr_vif.merMod <- postr_vif.lm

#' @rdname postr_vif
#' @export
pr_vif <- postr_vif
