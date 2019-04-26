getrate <- function(model, threshold, obs, class) {
  out <- sapply(threshold, function(t) {
    classification <- postr_classify(model, t)
    observed <- postr_observed(model)
    sum(classification == class & observed == obs)/sum(observed == obs)
  })
  out
}
