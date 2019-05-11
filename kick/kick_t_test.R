library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(123)

y <- rnorm(100, 5, 2)
x <- rnorm(100, 7, 6)

stan_data <- list(
                  N = length(y),
                  y = y,
                  x = x
)

model_t_test <- stan_model(file = "model/t_test.stan")

fit <- sampling(model_t_test, data = stan_data)

fit

diff <- rstan::extract(fit)$diff
p <- sum(ifelse(diff > 0, 1, 0)) / length(diff)
p

plot(density(diff))
