library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Loading data ------------------------------------------------------------
data("cars")
dataset <- cars

set.seed(123)
new_index <- sample(1:nrow(dataset), 20, replace=FALSE)

dataset_base <- dataset[-new_index,]
dataset_new <- dataset[new_index,]


# Kick Stan model ---------------------------------------------------------
# https://qiita.com/kilometer/items/8b81560c0efef5e0cee2

stan_data <- list(
  N = nrow(dataset_base),
  x = dataset_base$speed,
  y = dataset_base$dist,
  N2 = nrow(dataset_new),
  x2 = dataset_new$speed
)


fit <- stan(file = "model/fit_gaussian_process.stan",
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
A <- rstan::extract(fit)

y2_med <- apply(A$y2, 2, median)
y2_max <- apply(A$y2, 2, quantile, probs = 0.05)
y2_min <- apply(A$y2, 2, quantile, probs = 0.95)

dat_g <- data.frame(speed=dataset_new$speed, y2_med, y2_max, y2_min)
dat_g2 <- data.frame(speed=dataset_base$speed, dist=dataset_base$dist)

ggplot(dat_g, aes(speed, y2_med))+
  theme_classic()+
  geom_ribbon(aes(ymax = y2_max, ymin = y2_min), alpha = 0.2)+
  geom_line()+
  geom_point(data = dat_g2, aes(speed, dist))+
  xlab("speed") + ylab("dist")
