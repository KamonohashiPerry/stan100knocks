library(tidyverse)
library(rstan)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Data Import -------------------------------------------------------------
# https://cran.r-project.org/web/packages/brms/vignettes/brms_multivariate.html
data("BTdata", package = "MCMCglmm")
dataset <- BTdata
dataset <- dataset %>% mutate(sex = if_else( sex == "Male", 1, 0))
dataset <- dataset %>% select(-animal, -dam, -fosternest)


# Kick Stan model ---------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/multivariate-outcomes.html

stan_data <- list(N = nrow(dataset), # // num row
                  K = ncol(dataset %>% select(tarsus,back)), # // num vector
                  J = ncol(dataset %>% select(hatchdate,sex)), # // num regressor
                  x = as.matrix(dataset %>% select(-tarsus,-back)), # // regressor
                  y = as.matrix(dataset %>% select(-hatchdate,-sex)) # // vector
)

fit <- stan(file = "model/sur_model_cholesky.stan",
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

param_names <- c('mcmc', paste0('b', 1:4))
d_est <- data.frame(1:N_mcmc, ms$beta)
colnames(d_est) <- param_names
d_qua <- data.frame.quantile.mcmc(x=param_names[-1], y_mcmc=d_est[,-1])
d_melt <- reshape2::melt(d_est, id=c('mcmc'), variable.name='X')
d_melt$X <- factor(d_melt$X, levels=rev(levels(d_melt$X)))

p <- ggplot()
p <- p + theme_bw(base_size=18)
p <- p + coord_flip()
p <- p + geom_violin(data=d_melt, aes(x=X, y=value), fill='white', color='grey80', size=2, alpha=0.3, scale='width')
p <- p + geom_pointrange(data=d_qua, aes(x=X, y=p50, ymin=p2.5, ymax=p97.5), size=1)
p <- p + labs(x='parameter', y='value')
p <- p + scale_y_continuous(breaks=seq(from=-2, to=6, by=2))
p
