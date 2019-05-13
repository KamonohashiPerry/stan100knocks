library(tidyverse)
library(rstan)

dataset <- read_csv("dataset/data-poisson-binomial.csv")

stan_data <- list(N = nrow(dataset),
                  M_max = max(dataset$Y),
                  Y = as.vector(dataset$Y)
)

stan_model <- stan_model("model/discrete_parameter_poisson.stan")

fit <- sampling(object = stan_model, stan_data)

fit

ms <- rstan::extract(fit)

ggplot(data = data.frame(ms), aes(x = lambda)) + geom_density()
