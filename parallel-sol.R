library(foreach)
library(doParallel)
# simulate lm fit with t-distributed errors
# @params:
#   reps: number of test simulations
#   seed
#   true_coef: vector true model coefficients
#   df: distribution degrees of freedom
# @return: n x reps dimensional matrix,
#         where is the length of the true coefficients vector
simulate_parallel <- function(reps, seed, data,
                              true_coef = 0:ncol(data), df = 4) {
  set.seed(seed)
  coef <- matrix(0, nrow = length(true_coef), ncol = reps)

  design <- model.matrix(~., data = data)
  expected <- crossprod(t(design), true_coef)
  foreach(rep = seq_len(reps), .combine = "c") %dopar%
    simulate_once_parallel(design, expected, df)
  return(structure(coef, seed = seed))
}

simulate_once_parallel <- function(design, expected, df) {
  y <- simulate_response_parallel(expected, df)
  estimate_coef_parallel(design, y)
}

simulate_response_parallel <- function(expected, df) {
  return(expected + rt(length(expected), df = df))
}

estimate_coef_parallel <- function(design, y) {
  model <- lm.fit(design, y)
  unname(coef(model))
}
registerDoParallel(3)
