library(tidyverse)
library(rstan)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Data Import -------------------------------------------------------------
dataset <- data.frame(Loblolly)
dataset$Seed <- as.numeric(dataset$Seed)

# 偶数のインデックスの取得
n <- seq(1, nrow(dataset), by = 2)
# 偶数と奇数でデータセットを取得
dataset_raw <- dataset[n,]
dataset_new <- dataset[-n,]


# Kick Stan model ---------------------------------------------------------

stan_data <- list(N = nrow(dataset_raw), # // num row
                  K = ncol(dataset_raw)-1, # // num predictors
                  x = as.matrix(dataset_raw %>% select(-height)), # // predictors
                  y = dataset_raw$height, # // outcomes
                  N_new = nrow(dataset_new) , # num new row
                  x_new = as.matrix(dataset_new %>% select(-height)) # new predictors
)

fit <- stan(file = "model/prediction_linear_model.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


# Diagnose ----------------------------------------------------------------

summary(fit)

traceplot(fit)

launch_shinystan(fit)


# Visualization -----------------------------------------------------------
# グループごとの係数の推定値を可視化する（作成中）
source('kick/common.R')

ms <- rstan::extract(fit)
N_mcmc <- length(ms$lp__)

param_names <- c('mcmc', paste0('b', 1:(ncol(dataset_raw)-1)))
d_est <- data.frame(1:N_mcmc, ms$beta)
d_est <- d_est[,1:(ncol(dataset_raw))]
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



# Compare Actual Value ----------------------------------------------------
dataset_new_pred <- dataset_new %>% mutate(prediction_value = apply(as.matrix(ms$y_new), 2, mean))

melt_dataset <- reshape2::melt(dataset_new_pred,id.vars="age")
melt_dataset <- melt_dataset %>% filter(variable %in% c("prediction_value", "height") )

g <- ggplot(melt_dataset, aes(x = age, y = value, colour=variable))
g <- g + geom_point()
g <- g + geom_smooth(method = "lm")
plot(g)

