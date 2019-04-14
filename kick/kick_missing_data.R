library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

y_obs <- data.frame( y_value = rnorm(1000, mean = 0, sd = 1))
y_obs <- y_obs %>% mutate( random_value = runif(nrow(y_obs),min = 0, max = 1 ))
y_obs <- y_obs %>% mutate( missing_flag = if_else(random_value <= 0.5, 1, 0))
y_obs <- y_obs %>% mutate( y_obs_with_miss = if_else(missing_flag == 0, y_value,NULL))


# Kick Stan model ---------------------------------------------------------

stan_data <- list(N_obs = nrow(y_obs) - sum(y_obs$missing_flag),
                  N_mis = sum(y_obs$missing_flag),
                  y_obs = as.matrix(y_obs %>% filter(missing_flag == 0) %>% select(y_value))[,1])


fit <- stan(file = "model/missing_data.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


# Diagnose ----------------------------------------------------------------

summary(fit)

traceplot(fit)

launch_shinystan(fit)

