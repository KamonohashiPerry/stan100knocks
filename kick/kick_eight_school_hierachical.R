library(tidyverse)
library(rstan)

options(mc.cores = parallel::detectCores()) # parallelize
rstan_options(auto_write = TRUE)  # store compiled stan model


# Preparing the data for modeling -----------------------------------------
schools.data <- list(
  n = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)


# Sampling from the posterior distribution --------------------------------
fit1 <- stan(
  file = "model/schools.stan",  # Stan program
  data = schools.data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  refresh = 1000          # show progress every 'refresh' iterations
)


# Model interpretation ----------------------------------------------------
print(fit1) # optional parameters: pars, probs


# specify the params to plot via pars
plot(fit1, pars = "theta")



# MCMC diagnostics --------------------------------------------------------
# diagnostics:
traceplot(fit1, pars = c("mu", "tau"), inc_warmup = TRUE, nrow = 2)


# retrieve matrix of iterations, chains, and parameters
chain.data <- extract(fit1, permuted = FALSE) 
print(chain.data[1,,]) # parameters of all chains for the 1st of 1000 iterations


library(shinystan)
launch_shinystan(fit1)

