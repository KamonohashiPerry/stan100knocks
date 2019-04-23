library(rstan)
library(tidyverse)

dataset <- read_csv("dataset/data-protein.csv")

idx <- grep('<', dataset$Y)
Y_obs <- as.numeric(dataset[-idx, ]$Y)
L <- as.numeric(sub('<', '', dataset[idx, ]$Y))[1]

stan_data <- list(N_obs = length(Y_obs),
                  N_cens = length(idx),
                  Y_obs = Y_obs,
                  L = L)

fit <- stan(file='model/truncated_normal.stan',
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))

# Diagnose ----------------------------------------------------------------

summary(fit)

traceplot(fit)

ms <- rstan::extract(fit)

summary(ms$mu)
summary(as.numeric(dataset[-idx,]$Y))

