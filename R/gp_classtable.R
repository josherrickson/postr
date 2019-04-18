gp_classtable <- function(model, threshold, ...) {
  stopifnot(is(model, "glm"))
  stopifnot(model$family$family == "binomial")

  observed <- model$data[,as.character(attributes(model$terms)$variables[[2]])]
  classified <- as.numeric(gp_classify(model, threshold))

  table(Observed = factor(observed, levels = c(0,1)),
        Classified = factor(classified, levels = c(0,1)),
        ...)
}
