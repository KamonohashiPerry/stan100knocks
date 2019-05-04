library(rethinking)
library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Loading data ------------------------------------------------------------
data("WaffleDivorce")
dataset <- WaffleDivorce


# Kick Stan model ---------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/bayesian-measurement-error-model.html

stan_data <- list(
  N = nrow(dataset),
  y = dataset$Divorce,
  x_meas = dataset$Marriage,
  tau = 1.3994
)


fit <- stan(file = "model/measurement_error.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


# Diagnose ----------------------------------------------------------------
summary(fit)

traceplot(fit)

launch_shinystan(fit)

