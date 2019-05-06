library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Loading data ------------------------------------------------------------
set.seed(123)
dataset <- data.frame( value = rnorm(100, mean = 300, sd = 30))


# Kick Stan model ---------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/simulating-from-a-gaussian-process.html

stan_data <- list(
              N = nrow(dataset),
              x = dataset$value
              )


fit <- stan(file = "model/gaussian_process.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


# Diagnose ----------------------------------------------------------------
summary(fit)

traceplot(fit)

launch_shinystan(fit)

summary_table <- data.frame(summary(fit)$summary)
ggplot(data = data.frame(Rhat = summary_table$Rhat), aes(Rhat)) + geom_histogram()



# Visualization -----------------------------------------------------------
ms <- rstan::extract(fit)
source('kick/common.R')
d_est <- data.frame.quantile.mcmc(x=dataset$value, y_mcmc=ms$y, probs=c(0.1, 0.25, 0.5, 0.75, 0.9))
p <- ggplot.5quantile(data=d_est)
p <- p + labs(x='input', y='output')
p
