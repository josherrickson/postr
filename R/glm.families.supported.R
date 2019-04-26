.glm.families.supported <- function(model, supported) {
  if (!(model$family$family %in% supported)) {
    stop(paste("glm family must be one of", supported))
  }
}
