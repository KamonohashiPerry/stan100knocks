library(bayesm)
library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Loading data ------------------------------------------------------------
data("cheese")
dataset <- cheese
set.seed(123)
dataset <- sample(dataset$PRICE,size = 1000,replace = TRUE)

# Kick Stan model ---------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/summing-out-the-responsibility-parameter.html

stan_data <- list(
  K = 3,
  N = length(dataset),
  y = dataset
)


fit <- stan(file = "model/parameters_mixture.stan",
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
estimated_mu <- data.frame(ms$mu)
estimated_mu <- reshape2::melt(data = estimated_mu)

ggplot(data = estimated_mu,
       aes(x = value, fill = variable)) + geom_histogram()

