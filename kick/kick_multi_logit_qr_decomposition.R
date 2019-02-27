library(tidyverse)
library(rstan)
library(shinystan)

# Dataset Import ----------------------------------------------------------
# https://www.kaggle.com/alphaepsilon/housing-prices-dataset#train.csv
dataset <- read_csv(file = "dataset/housing_prices_dataset_train.csv")
dataset <- dataset %>% select(SalePrice, YearBuilt, LotArea, BsmtFullBath)
dataset <- dataset %>% mutate_each_(funs(scale),
                                    vars=c("YearBuilt", "LotArea", "BsmtFullBath"))


dataset <- dataset %>% mutate(high_price_flag = if_else(SalePrice > 200000, 3,
                                                        if_else(SalePrice > 150000, 2,
                                                                1)))



# Kick Stan ---------------------------------------------------------------
# https://mc-stan.org/docs/2_18/stan-users-guide/parameterizing-centered-vectors.html
N <- nrow(dataset)
D <- ncol(dataset) - 2

data <- list(N = N,
             K = length(table(dataset$high_price_flag)),
             D = D,
             y = dataset$high_price_flag,
             x = as.matrix(dataset %>% select(-SalePrice,-high_price_flag)))

fit <- stan(file = "model/multi_logit_regression_qr_decomposition.stan",
            data = data,
            seed = 1234)



# Diagnose ----------------------------------------------------------------

summary(fit)

traceplot(fit)

launch_shinystan(fit)


# Visualization -----------------------------------------------------------
source('kick/common.R')

ms <- rstan::extract(fit)
N_mcmc <- length(ms$lp__)

param_names <- c('mcmc', paste0('b', 1:D))
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

