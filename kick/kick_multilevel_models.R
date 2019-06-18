# package
# https://github.com/rmcelreath/rethinking

library(rethinking)
library(shinystan)
library(loo)
library(dplyr)
library(tidyr)


# Import Data -------------------------------------------------------------
data(reedfrogs)
dataset <- reedfrogs

str(dataset)


# Kick Stan Code ----------------------------------------------------------
# https://github.com/ssp3nc3r/rethinking/blob/master/chapter12.Rmd
# make the tank cluster variable

dataset$tank <- 1:nrow(dataset)

stan_data <- list(
                  N = nrow(dataset),
                  N_tanks = max(dataset$tank),
                  tank_id = dataset$tank,
                  density = dataset$density,
                  surv = dataset$surv
                  )

stan_code1 <- stan_model("model/multilevel_models_001.stan")
fit12_1 <- sampling(stan_code1, data = stan_data,
                    iter = 2000, chains = 4, cores = 2)


stan_code2 <- stan_model("model/multilevel_models_002.stan")
fit12_2 <- sampling(stan_code2, data = stan_data,
                    iter = 2000, chains = 4, cores = 2)

# diagnose ----------------------------------------------------------------

fit12_1

summary_table <- data.frame(summary(fit12_1)$summary)
ggplot(data = data.frame(Rhat = summary_table$Rhat), aes(Rhat)) + geom_histogram()


rstan::traceplot(fit12_1)


# Information Criteria ----------------------------------------------------

library(loo)
ll12_1 <- extract_log_lik(fit12_1)
ll12_2 <- extract_log_lik(fit12_2)
reff12_1 <- relative_eff(ll12_1, chain_id = c(rep(1, 500), rep(2, 500)), cores =2)
reff12_2 <- relative_eff(ll12_2, chain_id = c(rep(1, 500), rep(2, 500)), cores =2)
waic12_1 <- waic(ll12_1, r_eff = reff12_1, cores = 2)
waic12_2 <- waic(ll12_2, r_eff = reff12_2, cores = 2)
loo::compare(waic12_1, waic12_2)
loo12_1 <- loo(ll12_1, r_eff = reff12_1, cores = 2)
loo12_2 <- loo(ll12_2, r_eff = reff12_1, cores = 2)
loo::compare(loo12_1, loo12_2)



# Visualization -----------------------------------------------------------

post12_2 <- as.data.frame(fit12_2, pars = c('alpha', 'sigma', 'a_tank'))
dataset$propsurv_est <- post12_2[,-c(1:2)] %>% colMeans %>% plogis
ggplot(dataset) + 
  geom_vline(xintercept = c(16.5, 32.5)) +
  geom_hline(yintercept = plogis(mean(post12_2$alpha)), linetype = 'dashed') +
  geom_point(aes(x = tank, y = propsurv), color = 'dodgerblue') +
  geom_point(aes(x = tank, y = propsurv_est), shape = 21) +
  scale_x_continuous(limits = c(0, 48), breaks = c(1, 16, 32, 48)) +
  annotate('text', x = 8, y = 0, label = 'Small tanks', hjust = 0.5) +
  annotate('text', x = 16+8, y = 0, label = 'Medium tanks', hjust = 0.5) +
  annotate('text', x = 32+8, y = 0, label = 'Large tanks', hjust = 0.5) +
  labs(x = 'Tank', y = 'Proportion survived')
