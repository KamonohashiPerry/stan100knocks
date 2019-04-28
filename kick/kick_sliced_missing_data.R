library(gtrendsR)
library(tidyverse)
library(rstan)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Data Import -------------------------------------------------------------

dataset <- gtrends(keyword = "熱海温泉",
                   time = "2009-01-01 2019-03-01")
class(dataset)

set.seed(123)
y_index <- ifelse(runif(length(y), min = 0,max = 1) >= 0.5, 1, 0)


y <- dataset$interest_over_time$hits

y_obs <- y[y_index > 0]
y_mis <- y[y_index == 0]
ii_obs <- which(y_index > 0)
ii_mis <- which(y_index == 0)

# Kick Stan model ---------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/sliced-missing-data.html

stan_data <- list(N_obs = length(y_obs),
                  N_mis = length(y_mis),
                  ii_obs = ii_obs,
                  ii_mis = ii_mis,
                  y_obs = y_obs
)


fit <- stan(file = "model/sliced_missing_data.stan",
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
y_estimated <- rstan::extract(fit)$y %>% apply(2,mean)
df <- data.frame(y=y, y_estimated=y_estimated,date=dataset$interest_over_time$date)
df_melt <- reshape2::melt(df, id.vars="date")
ggplot(data = df_melt, aes(x = date, y = value, color=variable)) + geom_line()

