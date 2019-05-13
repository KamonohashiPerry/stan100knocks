library(tidyverse)
library(rstan)

dataset <- read_csv("dataset/data-coin.csv")

stan_data <- list(N = nrow(dataset),
                  Y = as.vector(dataset$Y)
                  )

stan_model <- stan_model("model/discrete_parameter.stan")

fit <- sampling(object = stan_model, stan_data)

fit

ms <- rstan::extract(fit)

ggplot(data = data.frame(ms), aes(x = q)) + geom_histogram()

