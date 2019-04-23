library(rstan)
library(tidyverse)

dataset <- read_csv("dataset/data-outlier.csv")

plot(dataset$X, dataset$Y)


X_new <- seq(from=0, to=11, length=100)

stan_data <- list(N = nrow(dataset),
                  X = dataset$X,
                  Y = dataset$Y,
                  N_new = length(X_new),
                  X_new = X_new)

fit <- stan(file='model/linear_estimation_with_outlier.stan',
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


fit_normal <- stan(file='model/linear_estimation_with_outlier_normal.stan',
                   data = stan_data,
                   iter = 2000,
                   chains = 4,
                   seed = 1234,
                   control = list(max_treedepth = 15,adapt_delta=0.99))

# Diagnose ----------------------------------------------------------------

summary(fit)

traceplot(fit)


ms <- rstan::extract(fit)
source('kick/common.R')
d_est <- data.frame.quantile.mcmc(x=X_new, y_mcmc=ms$y_new)
p <- ggplot.5quantile(data=d_est)
p <- p + geom_point(data=dataset, aes(x=X, y=Y), shape=1, size=3)
p <- p + labs(x='X', y='Y')
p <- p + coord_cartesian(xlim=c(-0.2, 11.2), ylim=c(-25, 75))
p


ms <- rstan::extract(fit_normal)
source('kick/common.R')
d_est <- data.frame.quantile.mcmc(x=X_new, y_mcmc=ms$y_new)
p <- ggplot.5quantile(data=d_est)
p <- p + geom_point(data=dataset, aes(x=X, y=Y), shape=1, size=3)
p <- p + labs(x='X', y='Y')
p <- p + coord_cartesian(xlim=c(-0.2, 11.2), ylim=c(-25, 75))
p

