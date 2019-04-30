library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(123)
y_obs <- data.frame( y_value = rnorm(1000, mean = 300, sd = 30))
N_cens <- sum(y_obs$y_value  < 250)
y_obs_cencerd <- y_obs %>% mutate( y_value = if_else(y_value < 250, 250, y_value))

# Kick Stan model ---------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/censored-data.html

stan_data <- list(
  N_obs = nrow(y_obs_cencerd),
  N_cens = N_cens,
  y_obs = as.matrix(y_obs_cencerd$y_value)[,1]
)


fit <- stan(file = "model/left_censored_data.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))



# Diagnose ----------------------------------------------------------------

summary(fit)

traceplot(fit)

launch_shinystan(fit)


y_obs_censored_estimated <- y_obs
y_obs_censored_estimated <- y_obs_censored_estimated$y_value[250 <= y_obs_censored_estimated$y_value]
y_obs_censored_estimated <- append(y_obs_censored_estimated, as.numeric(data.frame(rstan::extract(fit)$y_cens)[1,]))

summary(y_obs_censored_estimated)
summary(y_obs$y_value)
summary(y_obs_cencerd$y_value)


# Visualization -----------------------------------------------------------
y_estimated <- as.vector(rstan::extract(fit)$y_cens)
y_estimated <- data.frame(y_estimated = as.matrix(y_estimated))
y_obs_under250 <- data.frame(y_value = y_obs$y_value[y_obs$y_value > 250])

ggplot(data = y_estimated,aes(x = y_estimated, color ="r")) + 
  geom_density() + geom_density(data = y_obs_under250,
                                aes(x = y_value, color="b"))
