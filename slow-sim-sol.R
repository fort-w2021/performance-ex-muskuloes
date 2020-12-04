# simulate lm fit with t-distributed errors
# @params:
#   reps: number of test simulations
#   seed
#   true_coef: vector true model coefficients
#   df: distribution degrees of freedom
# @return: n x reps dimensional matrix,
#         where is the length of the true coefficients vector
simulate_opt <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  set.seed(seed)
  coef <- matrix(0, nrow = length(true_coef), ncol = reps)

  design <- model.matrix(~., data = data)
  expected <- crossprod(t(design), true_coef)
  for (rep in seq_len(reps)) {
    coef[, rep] <- simulate_once_opt(design, expected, df)
  }
  return(structure(coef, seed = seed))
}

simulate_once_opt <- function(design, expected, df) {
  y <- simulate_response_opt(expected, df)
  estimate_coef_opt(design, y)
}

simulate_response_opt <- function(expected, df) {
  return(expected + rt(length(expected), df = df))
}

estimate_coef_opt <- function(design, y) {
  model <- lm.fit(design, y)
  unname(coef(model))
}
