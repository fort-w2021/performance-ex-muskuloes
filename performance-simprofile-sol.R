library(profvis)
library(rbenchmark)
source("slow-sim.R")
source("slow-sim-sol.R")
source("parallel-sol.R")

set.seed <- 232323
observations <- 5000
covariates <- 10
testdata <- as.data.frame(
  matrix(rnorm(observations * covariates),
    nrow = observations
  )
)

test <- simulate(reps = 100, seed = 20141028, data = testdata)

# a)
# cbind isn't very efficient as it does a copy of the data, use `[, rep]`.
# A lot of time is spent computing the design matrix and the expected
# output during every iteration.
# The `lm` method is quite inefficient as well
profvis(test <- simulate(reps = 100, seed = 20141028, data = testdata))

benchmark(
  simulate(reps = 100, seed = 20141028, data = testdata),
  simulate_opt(reps = 100, seed = 20141028, data = testdata),
  simulate_parallel(reps = 100, seed = 20141028, data = testdata),
  columns = c("test", "elapsed", "relative"),
  replications = 1,
  order = "elapsed"
)
system.time(test <- simulate(reps = 100, seed = 20141028, data = testdata))
# system.time(test_opt <-
# simulate_opt(reps = 100, seed = 20141028, data = testdata))
# system.time(test_parallel <-
# simulate_parallel(reps = 100, seed = 20141028, data = testdata))
