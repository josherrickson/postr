#' @export
postr_classificationtable.glmerMod <- postr_classificationtable.glm

#' @export
postr_classify.glmerMod <- postr_classify.glm

#' @export
postr_observed.glmerMod <- function(model) {
  model@frame[,1]
}

#' @export
postr_tpr.glmerMod <- postr_tpr.glm

#' @export
postr_tnr.glmerMod <- postr_tnr.glm

#' @export
postr_fpr.glmerMod <- postr_fpr.glm

#' @export
postr_fnr.glmerMod <- postr_fnr.glm

#' @export
postr_ROC.glmerMod <- postr_ROC.glm

#' @export
postr_AUC.glmerMod <- postr_AUC.glm
