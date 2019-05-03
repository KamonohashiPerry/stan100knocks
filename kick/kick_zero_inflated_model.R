library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Loading data ------------------------------------------------------------
prob_drink <- 0.2
rate_work <- 1
N <- 365
set.seed(123)
drink <- rbinom(N, 1, prob_drink)
y <- (1 - drink)*rpois(N, rate_work)


# Kick Stan model ---------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/zero-inflated-section.html

stan_data <- list(
  N = length(y),
  y = y
)


fit <- stan(file = "model/zero_inflated_model.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


fit_poisson <- stan(file = "model/poisson.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


# Diagnose ----------------------------------------------------------------
summary(fit)

traceplot(fit)

launch_shinystan(fit)


# Visualization -----------------------------------------------------------
ms <- rstan::extract(fit)
estimated_theta <- data.frame(value=ms$theta)
estimated_lambda <- data.frame(value=ms$lambda)

ggplot(data = estimated_theta,
       aes(x = value)) + geom_histogram()

ggplot(data = estimated_lambda,
       aes(x = value)) + geom_histogram()


ms_poisson <- rstan::extract(fit_poisson)
estimated_lambda_poisson <- data.frame(value=ms_poisson$lambda)

ggplot(data = estimated_lambda_poisson,
       aes(x = value)) + geom_histogram()
