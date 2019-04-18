getrate <- function(model, threshold, obs, class) {
  stopifnot(is(model, "glm"))
  stopifnot(model$family$family == "binomial")

  classification <- glmpost::gp_classify(model, threshold)
  observed <- model$data[,as.character(attributes(model$terms)$variables[[2]])]
  sum(classification == class & observed == obs)/sum(observed == obs)
}
