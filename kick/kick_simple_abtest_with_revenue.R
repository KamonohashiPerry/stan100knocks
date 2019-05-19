library("rstan")
library("bayesplot")
library("tidyverse")
library("lubridate")


# Data --------------------------------------------------------------------
stan_data <- list(
                  outcomes = 4,
                  payoffs = c(79, 49, 25, 0),
                  n_A = c(10, 46, 80, 864),
                  n_B = c(49, 84, 200, 1667),
                  a_A = rep(1, 4),
                  a_B = rep(1, 4)
                )


# Kick Stan Code ----------------------------------------------------------
# https://jrnold.github.io/presentation-2018-11-05/abtest.html


fit <- rstan::stan(file = "model/simple_abtest_with_revenue.stan",
                   data = stan_data,
                   iter = 2000,
                   chains = 4,
                   seed = 1234,
                   control = list(max_treedepth = 15,adapt_delta=0.99)
)



# diagnose ----------------------------------------------------------------

fit

summary_table <- data.frame(summary(fit)$summary)
ggplot(data = data.frame(Rhat = summary_table$Rhat), aes(Rhat)) + geom_histogram()

rstan::traceplot(fit, pars = c("revenue_A", "revenue_B", "revenue_diff"))

bayesplot::mcmc_acf(as.matrix(fit), pars = c("revenue_A", "revenue_B", "revenue_diff"))

bayesplot::mcmc_areas(as.matrix(fit), pars = c("revenue_A", "revenue_B", "revenue_diff"), prob = 0.95)

bayesplot::mcmc_areas_ridges(as.matrix(fit), pars = c("revenue_A", "revenue_B", "revenue_diff"))

bayesplot::mcmc_intervals(as.matrix(fit), pars = c("revenue_A", "revenue_B", "revenue_diff"))


# Calculate the probability -----------------------------------------------

pi_A <- drop(rstan::extract(fit, "pi_A")[[1]])
pi_B <- drop(rstan::extract(fit, "pi_B")[[1]])
mean(pi_A > pi_B)
mean(pi_A < pi_B)
