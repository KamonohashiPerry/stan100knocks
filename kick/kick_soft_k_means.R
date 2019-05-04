library(bayesm)
library(rstan)
library(tidyverse)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Loading data ------------------------------------------------------------
data("iris")
dataset <- iris

# Kick Stan model ---------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/soft-k-means.html

stan_data <- list(
  N = nrow(dataset),
  D = ncol(dataset[, -ncol(dataset)]),
  K = 3,
  y = dataset[, -ncol(dataset)]
)


fit <- stan(file = "model/soft_k_means.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


# Diagnose ----------------------------------------------------------------
summary(fit)

traceplot(fit)

launch_shinystan(fit)
