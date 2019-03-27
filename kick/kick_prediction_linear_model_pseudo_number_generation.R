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
# https://mc-stan.org/docs/2_18/stan-users-guide/prediction-forecasting-and-backcasting.html

stan_data <- list(N = nrow(dataset_raw), # // num row
                  x = as.numeric(dataset_raw$age), # // predictors
                  y = dataset_raw$height, # // outcomes
                  N_tilde = nrow(dataset_new) , # num new row
                  x_tilde = as.numeric(dataset_new$age)  # new predictors
)

fit <- stan(file = "model/prediction_linear_model_pseudo_number_generation.stan",
            data = stan_data,
            iter = 2000,
            chains = 4,
            seed = 1234,
            control = list(max_treedepth = 15,adapt_delta=0.99))


# Diagnose ----------------------------------------------------------------

summary(fit)$summary

traceplot(fit)

launch_shinystan(fit)

# Compare Actual Value ----------------------------------------------------
ms <- rstan::extract(fit)
dataset_new_pred <- dataset_new %>% mutate(prediction_value = apply(as.matrix(ms$y_tilde), 2, mean))

melt_dataset <- reshape2::melt(dataset_new_pred,id.vars="age")
melt_dataset <- melt_dataset %>% filter(variable %in% c("prediction_value", "height") )

g <- ggplot(melt_dataset, aes(x = age, y = value, colour=variable))
g <- g + geom_point()
g <- g + geom_smooth(method = "lm")
plot(g)
