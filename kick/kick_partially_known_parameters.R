library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

y_obs <- data.frame( y_value1 = rnorm(1000, mean = 0, sd = 1),
                     y_value2 = rnorm(1000, mean = 0, sd = 2))


# Kick Stan model ---------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/partially-known-parameters-section.html

stan_data <- list(
  N = nrow(y_obs),
  y = as.matrix(y_obs),
  var1 = var(y_obs$y_value1),
  var2 = var(y_obs$y_value2)
)


fit <- stan(file = "model/partially_known_parameters.stan",
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
N_mcmc <- length(ms$lp__)

param_names <- c('mcmc', paste0('mu'), paste0('cov'))
d_est <- data.frame(1:N_mcmc, as.numeric(ms$mu), as.numeric(ms$cov))
colnames(d_est) <- param_names
d_qua <- data.frame.quantile.mcmc(x=param_names[-1], y_mcmc=d_est[,-1])
d_melt <- reshape2::melt(d_est, id.vars=c('mcmc'), variable.name='X', value.name="value")
d_melt$X <- factor(d_melt$X, levels=rev(levels(d_melt$X)))

p <- ggplot()
p <- p + theme_bw(base_size=18)
p <- p + coord_flip()
p <- p + geom_violin(data=d_melt, aes(x=X, y=value), fill='white', color='grey80', size=2, alpha=0.3, scale='width')
p <- p + geom_pointrange(data=d_qua, aes(x=X, y=p50, ymin=p2.5, ymax=p97.5), size=1)
p <- p + labs(x='parameter', y='value')
p <- p + scale_y_continuous(breaks=seq(from=-2, to=6, by=2))
p

