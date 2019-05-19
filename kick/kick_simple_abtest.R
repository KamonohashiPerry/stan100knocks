library("rstan")
library("bayesplot")
library("tidyverse")
library("lubridate")


# Data --------------------------------------------------------------------
visitors = c(A = 1300, B = 1275)
conversions = c(A = 120, B = 125)


# Kick Stan Code ----------------------------------------------------------
# https://jrnold.github.io/presentation-2018-11-05/abtest.html

stan_data <- list(visitors_A = visitors["A"],
                  visitors_B = visitors["B"],
                  conversions_A = conversions["A"],
                  conversions_B = conversions["B"]
                  )


fit <- rstan::stan(file = "model/simple_abtest.stan",
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

rstan::traceplot(fit, par = c("eta_A","eta_B","pi_diff"))

bayesplot::mcmc_acf(as.matrix(fit), pars = c("pi_diff","eta_diff","lift"))

bayesplot::mcmc_areas(as.matrix(fit), pars = c("pi_diff","eta_diff","lift"), prob = 0.95)




# Calculate the probability -----------------------------------------------

pi_A <- drop(rstan::extract(fit, "pi_A")[[1]])
pi_B <- drop(rstan::extract(fit, "pi_B")[[1]])
mean(pi_A > pi_B)
mean(pi_A < pi_B)
