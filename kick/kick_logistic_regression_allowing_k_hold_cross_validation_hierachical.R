library(tidyverse)
library(rstan)
library(GGally)
library(shinystan)
library(pbmcapply)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

wine_dataset <- read.csv("dataset/winequality-red.csv")

# Visualization -----------------------------------------------------------
ggpairs(wine_dataset)


# Making group -----------------------------------------------------------
wine_dataset <- wine_dataset %>% mutate(citric_acid_group = if_else(citric.acid < 0.2, 1,
                                                                    if_else(citric.acid < 0.4, 2, 3)))

wine_dataset <- wine_dataset %>% select(-citric.acid)


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

x_train_group <- x_train[,ncol(x_train)]
x_test_group <- x_test[,ncol(x_test)]

x_train <- x_train[,1:(ncol(x_train)-1)]
x_test <- x_test[,1:(ncol(x_test)-1)]

x_train <- scale(x_train)
x_test <- scale(x_test)


N <- length(y_train) # sample size
n_fold <- 10 # number of folds
K <- ncol(x_train) #n umber of predictors

# Create cross validation data --------------------------------------------
# create 10 folds of data
hh <- kfold_split_random(n_fold, N) #hh index the fold ID of each data point
holdout_k <- matrix(0, nrow = N, ncol = n_fold)
for(i in 1:N) holdout_k[i, hh[i]] <- 1

# turn into a list
holdout_k <- split(holdout_k,rep(1:ncol(holdout_k),each=nrow(holdout_k)))


# the basic data object
data_m <- list(N=N,
               K=K,
               M = length(unique(x_train_group)),
               citric_acid_group = x_train_group,
               X=x_train,
               y=y_train
               )

# create a list of data list
data_l <- rep(list(data_m),n_fold)
# add the holdout index to it
for(i in 1:n_fold) data_l[[i]]$holdout <- holdout_k[[i]]


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



# Calculate Mean AUC ------------------------------------------------------
## hierachical model
set.seed(123)
cv_mean_auc <- NULL
for (i in 1:n_fold){
  ext_fit <- extract(ss[[i]]) # choose 1 chunk
  
  # グループによって推定したパラメータが違う
  coef_list <- NULL
  group_list <- data_l[[i]]$citric_acid_group[data_l[[i]]$holdout > 0]
  for(j in 1:length(group_list)){
    coef_list <- rbind(coef_list, colMeans(ext_fit$beta[,group_list[j],]))
  }
  
  lin_comb <- rowSums(data_l[[i]]$X[data_l[[i]]$holdout > 0, ] * coef_list)
  prob <- 1/(1 + exp(-lin_comb))
  pred_value <- rbinom(sum(data_l[[i]]$holdout), 1, prob)
  
  # Syntax (response, predictor):
  auc = pROC::auc(data_l[[i]]$y[data_l[[i]]$holdout > 0], pred_value)[1]
  cv_mean_auc <- append(cv_mean_auc, auc)
}

cv_mean_auc
mean(cv_mean_auc)



# Prediction --------------------------------------------------------------
# Choose best model in cross-validation
ext_fit <- extract(ss[[1]]) # choose 1 chunk

# グループによって推定したパラメータが違う
beta_post <- NULL
group_list <- x_test_group

for(j in 1:length(group_list)){
  beta_post <- rbind(beta_post, colMeans(ext_fit$beta[,group_list[j],]))
}

lin_comb <- rowSums(x_test * beta_post)
prob <- 1/(1 + exp(-lin_comb))
pred_value <- rbinom(nrow(x_test), 1, prob)


# Syntax (response, predictor):
auc = pROC::auc(y_test, pred_value)[1]
auc

