library(tidyverse)
library(rstan)
library(GGally)
library(shinystan)
library(loo)
library(pbmcapply)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

wine_dataset <- read.csv("dataset/winequality-red.csv")

# Visualization -----------------------------------------------------------
ggpairs(wine_dataset)


# Split Train and Test ----------------------------------------------------
## 75% of the sample size
smp_size <- floor(0.8 * nrow(wine_dataset))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(wine_dataset)), size = smp_size)

train <- wine_dataset[train_ind, ]
test <- wine_dataset[-train_ind, ]

y <- if_else(wine_dataset$quality > 5, 1, 0)
x <- as.matrix(wine_dataset %>% select(-quality))

y_train <- y[train_ind]
y_test <- y[-train_ind]

x_train <- x[train_ind,]
x_test <- x[-train_ind,]

x_train <- scale(x_train)
x_test <- scale(x_test)


N <- length(y_train) # sample size
n_fold <- 10 # number of folds
K <- ncol(x_train) #n umber of predictors

# Create cross validation data --------------------------------------------
# create 10 folds of data
hh <- kfold_split_random(n_fold, N) #hh index the fold ID of each data point
holdout_10 <- matrix(0, nrow = N, ncol = n_fold)
for(i in 1:N) holdout_10[i, hh[i]] <- 1

# turn into a list
holdout_10 <- split(holdout_10,rep(1:ncol(holdout_10),each=nrow(holdout_10)))


# the basic data object
data_m <- list(N=N,
               K=K,
               X=x_train,
               y=y_train)

# create a list of data list
data_l <- rep(list(data_m),10)
# add the holdout index to it
for(i in 1:10) data_l[[i]]$holdout <- holdout_10[[i]]


# Define function ---------------------------------------------------------
# function to parrallelize all computations
# need at least two chains !!!
stan_kfold <- function(file, list_of_datas, chains, cores,...){
  library(pbmcapply)
  badRhat <- 1.1 # don't know why we need this?
  n_fold <- length(list_of_datas)
  model <- stan_model(file=file)
  # First parallelize all chains:
  sflist <- 
    pbmclapply(1:(n_fold*chains), mc.cores = cores, 
               function(i){
                 # Fold number:
                 k <- ceiling(i / chains)
                 s <- sampling(model, data = list_of_datas[[k]], 
                               chains = 1, chain_id = i)
                 return(s)
               })
  
  # Then merge the K * chains to create K stanfits:
  stanfit <- list()
  for(k in 1:n_fold){
    inchains <- (chains*k - (chains - 1)):(chains*k)
    #  Merge `chains` of each fold
    stanfit[[k]] <- sflist2stanfit(sflist[inchains])
  }  
  return(stanfit) 
}



# Kick the stan code ------------------------------------------------------
# run the functions
ss <- stan_kfold(file="model/logistic_regression_allowing_k_fold_cross_validation_hierachical.stan",
                 data_l,
                 chains=4,
                 cores=2)

ss_normal <- stan_kfold(file="model/logistic_regression_allowing_k_fold_cross_validation.stan",
                 data_l,
                 chains=4,
                 cores=2)


# Calculate Mean AUC ------------------------------------------------------
## hierachical model
set.seed(123)
cv_mean_auc <- NULL
for (i in 1:n_fold){
  ext_fit <- extract(ss[[i]]) # choose 1 chunk
  coef_fit_1 <- mean(ext_fit$alpha)
  coef_fit_2 <- colMeans(ext_fit$beta)
  
  lin_comb <- data_l[[i]]$X[data_l[[i]]$holdout > 0, ] %*% coef_fit_2 
                                + replicate(sum(data_l[[i]]$holdout), 1) * coef_fit_1
  prob <- 1/(1 + exp(-lin_comb))
  pred_value <- rbinom(sum(data_l[[i]]$holdout), 1, prob)

  # Syntax (response, predictor):
  auc = pROC::auc(data_l[[i]]$y[data_l[[i]]$holdout > 0], pred_value)[1]
  cv_mean_auc <- append(cv_mean_auc, auc)
}

cv_mean_auc
mean(cv_mean_auc)


## no hierachical model
set.seed(123)
cv_mean_auc_normal <- NULL
for (i in 1:n_fold){
  ext_fit <- extract(ss_normal[[i]]) # choose 1 chunk
  coef_fit_1 <- mean(ext_fit$alpha)
  coef_fit_2 <- colMeans(ext_fit$beta)
  
  lin_comb <- data_l[[i]]$X[data_l[[i]]$holdout > 0, ] %*% coef_fit_2 
  + replicate(sum(data_l[[i]]$holdout), 1) * coef_fit_1
  prob <- 1/(1 + exp(-lin_comb))
  pred_value <- rbinom(sum(data_l[[i]]$holdout), 1, prob)
  
  # Syntax (response, predictor):
  auc = pROC::auc(data_l[[i]]$y[data_l[[i]]$holdout > 0], pred_value)[1]
  cv_mean_auc_normal <- append(cv_mean_auc_normal, auc)
}

cv_mean_auc_normal
mean(cv_mean_auc_normal)


# Prediction --------------------------------------------------------------
# Choose best model in cross-validation
ext_fit <- extract(ss[[1]]) # choose 1 chunk

# Extract posteriod distributions
alpha_post <- ext_fit$alpha
beta_post <- ext_fit$beta

lin_comb <- replicate(nrow(x_test), 1)*mean(alpha_post) + x_test %*% colMeans(ext_fit$beta) 
prob <- 1/(1 + exp(-lin_comb))
pred_value <- rbinom(nrow(x_test), 1, prob)
# Syntax (response, predictor):
auc = pROC::auc(y_test, pred_value)[1]
auc

