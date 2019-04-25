getrate <- function(model, threshold, obs, class) {
  out <- sapply(threshold, function(t) {
    classification <- postr_classify(model, t)
    observed <- model$data[,as.character(attributes(model$terms)$variables[[2]])]
    sum(classification == class & observed == obs)/sum(observed == obs)
  })
  out
}
