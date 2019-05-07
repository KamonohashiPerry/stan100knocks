library(tidyverse)
library(survminer)
library(biostan)

library(rstan)
library(bayesplot)
library(tidybayes)
library(survival)

# Load a simple dataset ---------------------------------------------------
data(leukemia, package = "survival")
leukemia <- as_data_frame(leukemia)
leukemia


# Regular Kaplan-Meier plot -----------------------------------------------
km_fit <- survfit(Surv(time, status) ~ x, data = leukemia)
km_fit

ggsurvplot(km_fit,
           conf.int = TRUE,
           break.time.by = 20,
           risk.table = TRUE)





# kick stan code ----------------------------------------------------------

stan_weibull_survival_model_data <-
  list(
    ## Number of event individuals
    Nobs = sum(leukemia$status == 1),
    ## Number of censored individuals
    Ncen = sum(leukemia$status == 0),
    ## Number of covariates
    M_bg = 1,
    ## Times for event individuals
    yobs = leukemia$time[leukemia$status == 1],
    ## Times for censored individuals
    ycen = leukemia$time[leukemia$status == 0],
    ## Covariates for event individuals as a matrix
    Xobs_bg = matrix(as.numeric(leukemia$x == "Maintained")[leukemia$status == 1]),
    ## Covariates for censored individuals as a matrix
    Xcen_bg = matrix(as.numeric(leukemia$x == "Maintained")[leukemia$status == 0])
  )

stan_weibull_survival_model_fit <-
              rstan::stan(file = "model/weibull_fit.stan",
              data = stan_weibull_survival_model_data)


# diagnose ----------------------------------------------------------------

stan_weibull_survival_model_fit

summary_table <- data.frame(summary(stan_weibull_survival_model_fit)$summary)
ggplot(data = data.frame(Rhat = summary_table$Rhat), aes(Rhat)) + geom_histogram()

rstan::traceplot(stan_weibull_survival_model_fit, par = c("alpha","mu","beta_bg"))

bayesplot::mcmc_acf(as.matrix(stan_weibull_survival_model_fit), pars = c("alpha","mu","beta_bg[1]"))

bayesplot::mcmc_areas(as.matrix(stan_weibull_survival_model_fit), pars = c("alpha","mu","beta_bg[1]"), prob = 0.95)


stan_weibull_survival_model_draws <- tidybayes::tidy_draws(stan_weibull_survival_model_fit)
stan_weibull_survival_model_draws



treatment_assignment <- c(as.numeric(leukemia$x == "Maintained")[leukemia$status == 1],
                          as.numeric(leukemia$x == "Maintained")[leukemia$status == 0])
treatment_assignment_df <-
  data_frame(obs = 1:23,
             treatment = treatment_assignment)
treatment_assignment_df


stan_weibull_survival_model_draws_yhat_uncens <-
  stan_weibull_survival_model_draws %>%
  select(.chain, .iteration, .draw, starts_with("yhat_uncens")) %>%
  gather(key = key, value = yhat_uncens, starts_with("yhat_uncens")) %>%
  separate(col = key, sep = "uncens", into = c("key","obs")) %>%
  select(-key) %>%
  ## Avoid using regular expressions with square brackets (syntax highlighter broke).
  ## https://stringr.tidyverse.org/articles/stringr.html
  mutate(obs = as.integer(str_sub(obs, 2, -2))) %>%
  left_join(y = treatment_assignment_df)
stan_weibull_survival_model_draws_yhat_uncens


ggplot(data = stan_weibull_survival_model_draws_yhat_uncens,
       mapping = aes(x = yhat_uncens, color = factor(treatment))) +
  geom_density(n = 512*10) +
  coord_cartesian(xlim = c(0,160)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())



## Constructor for treatment-specific survival function
construct_survival_function <- function(alpha, mu, beta, x) {
  function(t) {
    sigma_i <- exp(-1 * (mu + beta * x) / alpha)
    exp(- (t / sigma_i)^alpha)
  }
}

## Random functions
stan_weibull_survival_model_survival_functins <-
  stan_weibull_survival_model_draws %>%
  select(.chain, .iteration, .draw, alpha, mu, `beta_bg[1]`) %>%
  ## Simplify name
  rename(beta = `beta_bg[1]`) %>%
  ## Construct realization of random functions
  mutate(`S(t|1)` = pmap(list(alpha, mu, beta), function(a,m,b) {construct_survival_function(a,m,b,1)}),
         `S(t|0)` = pmap(list(alpha, mu, beta), function(a,m,b) {construct_survival_function(a,m,b,0)}))
stan_weibull_survival_model_survival_functins



times <- seq(from = 0, to = 160, by = 0.1)
times_df <- data_frame(t = times)

## Try first realizations
stan_weibull_survival_model_survival_functins$`S(t|1)`[[1]](times[1:10])


stan_weibull_survival_model_survival_functins$`S(t|0)`[[1]](times[1:10])


## Apply all realizations
stan_weibull_survival_model_survival <-
  stan_weibull_survival_model_survival_functins %>%
  mutate(times_df = list(times_df)) %>%
  mutate(times_df = pmap(list(times_df, `S(t|1)`, `S(t|0)`),
                         function(df, s1, s0) {df %>% mutate(s1 = s1(t),
                                                             s0 = s0(t))})) %>%
  select(-`S(t|1)`, -`S(t|0)`) %>%
  unnest() %>%
  gather(key = treatment, value = survival, s1, s0) %>%
  mutate(treatment = factor(treatment,
                            levels = c("s1","s0"),
                            labels = c("Maintained","Nonmaintained")))

## Average on survival scale
stan_weibull_survival_model_survival_mean <-
  stan_weibull_survival_model_survival %>%
  group_by(treatment, t) %>%
  summarize(survival_mean = mean(survival),
            survival_95upper = quantile(survival, probs = 0.975),
            survival_95lower = quantile(survival, probs = 0.025))

ggplot(data = stan_weibull_survival_model_survival,
       mapping = aes(x = t, y = survival, color = treatment, group = interaction(.chain,.draw,treatment))) +
  geom_line(size = 0.1, alpha = 0.02) +
  geom_line(data = stan_weibull_survival_model_survival_mean,
            mapping = aes(y = survival_mean, group = treatment)) +
  geom_line(data = stan_weibull_survival_model_survival_mean,
            mapping = aes(y = survival_95upper, group = treatment),
            linetype = "dotted") +
  geom_line(data = stan_weibull_survival_model_survival_mean,
            mapping = aes(y = survival_95lower, group = treatment),
            linetype = "dotted") +
  facet_grid(. ~ treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())



## Average on parameter space
stan_weibull_survival_model_average_parameters <-
  stan_weibull_survival_model_draws %>%
  summarize(alpha = mean(alpha),
            mu = mean(mu),
            beta = mean(`beta_bg[1]`))
stan_weibull_survival_model_average_parameters


stan_weibull_average_params_survival1 <- with(stan_weibull_survival_model_average_parameters,
                                              construct_survival_function(alpha, mu, beta, 1))
stan_weibull_average_params_survival0 <- with(stan_weibull_survival_model_average_parameters,
                                              construct_survival_function(alpha, mu, beta, 0))
stan_weibull_average_params_survival <-
  data_frame(t = seq(from = 0, to = 160, by = 0.1),
             s1 = stan_weibull_average_params_survival1(t),
             s0 = stan_weibull_average_params_survival0(t)) %>%
  gather(key = treatment, value = survival, -t) %>%
  mutate(treatment = factor(treatment,
                            levels = c("s1","s0"),
                            labels = c("Maintained","Nonmaintained")))

stan_weibull_average_params_survival %>%
  ggplot(mapping = aes(x = t, y = survival, color = treatment, group = treatment)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())



ggplot(data = stan_weibull_survival_model_survival,
       mapping = aes(x = t, y = survival, color = treatment, group = interaction(.chain,.draw,treatment))) +
  geom_line(size = 0.1, alpha = 0.02) +
  geom_line(data = stan_weibull_survival_model_survival_mean,
            mapping = aes(y = survival_mean, group = treatment)) +
  geom_line(data = stan_weibull_average_params_survival,
            mapping = aes(group = treatment),
            linetype = "dotted") +
  facet_grid(. ~ treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank())
