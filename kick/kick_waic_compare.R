library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

sppnames <- c("afarensis", "africanus", "habilis", "boisei",
              "rudolfensis", "ergaster", "sapiens")

brainvolcc <- c(438, 452, 612, 521, 752, 872, 1350)
masskg     <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)

d <- data.frame( species = sppnames,
                 brain = brainvolcc, mass = masskg)



# Kick Stan Code ----------------------------------------------------------
# http://ushi-goroshi.hatenablog.com/entry/2017/12/24/225748


stan_data_6.1 <- list(N = nrow(d),
                      K = ncol(as.matrix(d$mass)),
                      x = as.matrix(d$mass),
                      y = as.vector(d$brain))

fit6.1 <- stan(file = "model/multiple_linear_regression.stan",
              data = stan_data_6.1,
              iter = 2000,
              chains = 4,
              seed = 1234,
              control = list(max_treedepth = 15,adapt_delta=0.99))

post_samples <- rstan::extract(fit6.1)

Des <- cbind(1, d$mass) # 計画行列（Design Matrix）
B   <- cbind(post_samples$alpha, post_samples$beta) # パラメータ（Beta）
tmp <- matrix(NA, length(post_samples$alpha), nrow(d)) # 6000行、 100列の行列
for (i in 1:nrow(d)) {
  tmp[, i] <- dnorm(d$brain[i], mean = B %*% Des[i, ], 
                    sd = post_samples$sigma)
}
lppd <- sum(log(colMeans(tmp)))
pwaic <- sum(apply(tmp, 2, var))
WAIC <- -2 * (lppd - pwaic)

WAIC


stan_data_6.2 <- list(N = nrow(d),
                      K = ncol(as.matrix(data.frame(d$mass, d$mass^2))),
                      x = as.matrix(data.frame(d$mass, d$mass^2)),
                      y = as.vector(d$brain))

fit6.2 <- stan(file = "model/multiple_linear_regression.stan",
               data = stan_data_6.2,
               iter = 2000,
               chains = 4,
               seed = 1234,
               control = list(max_treedepth = 15,adapt_delta=0.99))


stan_data_6.3 <- list(N = nrow(d),
                      K = ncol(as.matrix(data.frame(d$mass, d$mass^2, d$mass^3))),
                      x = as.matrix(data.frame(d$mass, d$mass^2, d$mass^3)),
                      y = as.vector(d$brain))

fit6.3 <- stan(file = "model/multiple_linear_regression.stan",
               data = stan_data_6.3,
               iter = 2000,
               chains = 4,
               seed = 1234,
               control = list(max_treedepth = 15,adapt_delta=0.99))

summary(fit6.3)


stan_data_6.4 <- list(N = nrow(d),
                      K = ncol(as.matrix(data.frame(d$mass, d$mass^2, d$mass^3, d$mass^4))),
                      x = as.matrix(data.frame(d$mass, d$mass^2, d$mass^3, d$mass^4)),
                      y = as.vector(d$brain))

fit6.4 <- stan(file = "model/multiple_linear_regression.stan",
               data = stan_data_6.4,
               iter = 2000,
               chains = 4,
               seed = 1234,
               control = list(max_treedepth = 15,adapt_delta=0.99))


stan_data_6.5 <- list(N = nrow(d),
                      K = ncol(as.matrix(data.frame(d$mass, d$mass^2, d$mass^3, d$mass^4, d$mass^5))),
                      x = as.matrix(data.frame(d$mass, d$mass^2, d$mass^3, d$mass^4, d$mass^5)),
                      y = as.vector(d$brain))

fit6.5 <- stan(file = "model/multiple_linear_regression.stan",
               data = stan_data_6.5,
               iter = 2000,
               chains = 4,
               seed = 1234,
               control = list(max_treedepth = 15,adapt_delta=0.99))

stan_data_6.6 <- list(N = nrow(d),
                      K = ncol(as.matrix(data.frame(d$mass, d$mass^2, d$mass^3, d$mass^4, d$mass^5, d$mass^6))),
                      x = as.matrix(data.frame(d$mass, d$mass^2, d$mass^3, d$mass^4, d$mass^5, d$mass^6)),
                      y = as.vector(d$brain))

fit6.6 <- stan(file = "model/multiple_linear_regression.stan",
               data = stan_data_6.6,
               iter = 2000,
               chains = 4,
               seed = 1234,
               control = list(max_treedepth = 15,adapt_delta=0.99))


stan_data_6.7 <- list(N = nrow(d),
                      K = ncol(as.matrix(data.frame(rep(1, nrow(d))))),
                      x = as.matrix(data.frame(rep(1, nrow(d)))),
                      y = as.vector(d$brain))

fit6.7 <- stan(file = "model/multiple_linear_regression.stan",
               data = stan_data_6.7,
               iter = 2000,
               chains = 4,
               seed = 1234,
               control = list(max_treedepth = 15,adapt_delta=0.99))







# standard regerssion -----------------------------------------------------

m6.1 <- lm( brain ~ mass, data = d)
summary(m6.1)
AIC(m6.1)

1 - var(resid(m6.1)) / var(d$brain)


m6.2 <- lm( brain ~ mass + I(mass^2), data = d)
summary(m6.2)

m6.3 <- lm( brain ~ mass + I(mass^2) + I(mass^3), data = d)

m6.4 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4), data = d)

m6.5 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5), data = d)

m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6), data = d)


plot(brainvolcc)
plot(predict(m6.4))

m6.7 <- lm( brain ~ 1, data = d)

plot(predict(m6.7))


## information entropy
p <- c(0.3, 0.7)
-sum( p * log(p))

p <- c(0.01, 0.99)
-sum( p * log(p))

