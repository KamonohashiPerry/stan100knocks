library(tidyverse)
library(rstan)
library(GGally)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

wine_dataset <- read.csv("http://ieor.berkeley.edu/~ieor265/homeworks/winequality-red.csv", sep=";" )

y <- wine_dataset$quality - 2
x <- as.matrix(wine_dataset %>% select(-quality))
x <- scale(x)

# Visualization -----------------------------------------------------------
ggpairs(wine_dataset)


# Estimation --------------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/ordered-logistic-section.html
N <- nrow(x)
J <- 6L
K <- ncol(x)

data_customer_list_test <- list(N=N,
                                J=J,
                                K=K,
                                y=y,
                                x=x)

fit <- stan(file = "model/ordered_probit_regression.stan",
            data = data_customer_list_test,
            iter = 1000,
            chains = 4
            )

summary(fit)
traceplot(fit)

# Convergence Check -------------------------------------------------------
launch_shinystan(fit)

# Result Plot -------------------------------------------------------------
source('kick/common.R')

ms <- rstan::extract(fit)
N_mcmc <- length(ms$lp__)


param_names <- c('mcmc', colnames(wine_dataset %>% select(-quality)))

d_est <- data.frame(1:N_mcmc, ms$b)
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
