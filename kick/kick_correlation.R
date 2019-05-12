library(rstan)
library(tidyverse)
library(MASS)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

mu <- c(0, 0)
Sigma <- matrix(c(1, 0.7, 0.7, 1), 2, 2)

set.seed(123)
y <- mvrnorm(25, mu, Sigma)


# cor(y)
# plot(y)

stan_data <- list(
                  N = nrow(y),
                  L = ncol(y),
                  y = y
)

model_corr <- stan_model(file = "model/correlation.stan")

fit <- sampling(model_corr, data = stan_data)

fit

print(fit, pars = "rho", digits=3)

rho <- rstan::extract(fit)$rho
plot(density(rho))


# 最頻値の計算
map_mcmc <- function(z){
                  density(z)$x[which.max(density(z)$y)]
            }
map_mcmc(rho)


