library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


d <- read_csv(file='dataset/data-aircon.csv')

# Kick Stan model ---------------------------------------------------------

N_new <- 60
X_new <- seq(from=-3, to=32, length=N_new)
stan_data <- list(N = nrow(d),
                  X=d$X,
                  Y=d$Y,
                  N_new=N_new,
                  X_new=X_new)

fit <- stan(file='model/non_linear_model.stan',
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
source('kick/common.R')

ms <- rstan::extract(fit)

d_est <- data.frame.quantile.mcmc(x=X_new, y_mcmc=ms$y_new)
p <- ggplot.5quantile(data=d_est, size=0.5)
p <- p + geom_point(data=d, aes(x=X, y=Y), shape=1, size=2)
p <- p + labs(x='X', y='Y')
p <- p + scale_y_continuous(breaks=seq(from=0, to=100, by=50), limits=c(0, 130))
p
