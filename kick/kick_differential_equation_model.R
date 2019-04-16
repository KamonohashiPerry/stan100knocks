library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


d <- read_csv(file='dataset/data-conc.csv')

# Kick Stan model ---------------------------------------------------------

T_new <- 60
Time_new <- seq(from=0, to=24, length=T_new)

stan_data <- list(T=nrow(d),
                  Time=d$Time,
                  Y=d$Y,
                  T_new=T_new,
                  Time_new=Time_new)

fit <- stan(file='model/differential_equation_model.stan',
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

d_est <- data.frame.quantile.mcmc(x=Time_new, y_mcmc=ms$y_new)
p <- ggplot.5quantile(data=d_est, size=0.5)
p <- p + geom_point(data=d, aes(x=Time, y=Y), shape=16, size=3)
p <- p + labs(x='Time (hour)', y='Y')
p <- p + scale_x_continuous(breaks=d$Time, limit=c(0, 24))
p <- p + ylim(-2.5, 16)
p

