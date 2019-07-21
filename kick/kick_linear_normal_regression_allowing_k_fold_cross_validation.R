library(rstan)
rstan_options(auto_write = TRUE)
library(pbmcapply)
library(loo)


N <- 100 #sample size
K <- 2 #number of predictors
n_fold <- 10 #number of folds


# Create cross validation data --------------------------------------------
# create 10 folds of data
hh <- kfold_split_random(n_fold, N) #hh index the fold ID of each data point
holdout_10 <- matrix(0, nrow = N, ncol = n_fold)
for(i in 1:N) holdout_10[i, hh[i]] <- 1

# turn into a list
holdout_10 <- split(holdout_10,rep(1:ncol(holdout_10),each=nrow(holdout_10)))


# Create dataset ----------------------------------------------------------
X <- cbind(rep(1,N),runif(N,-2,2))
y <- rnorm(N,X %*% c(1, 0.5),1)

# the basic data object
data_m <- list(N=N,
               K=K,
               X=X,
               y=y)
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
                               chains = 1, chain_id = i,...)
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

# extract log-likelihoods of held-out data
extract_log_lik_K <- function(list_of_stanfits, list_of_holdout, ...){
  require(loo)
  K <- length(list_of_stanfits)
  list_of_log_liks <- plyr::llply(1:K, function(k){
    extract_log_lik(list_of_stanfits[[k]],...)
  })
  # `log_lik_heldout` will include the loglike of all the held out data of all the folds.
  # We define `log_lik_heldout` as a (samples x N_obs) matrix
  # (similar to each log_lik matrix)
  log_lik_heldout <- list_of_log_liks[[1]] * NA
  for(k in 1:K){
    log_lik <- list_of_log_liks[[k]]
    samples <- dim(log_lik)[1] 
    N_obs <- dim(log_lik)[2]
    # This is a matrix with the same size as log_lik_heldout
    # with 1 if the data was held out in the fold k
    heldout <- matrix(rep(list_of_holdout[[k]], each = samples), nrow = samples)
    # Sanity check that the previous log_lik is not being overwritten:
    if(any(!is.na(log_lik_heldout[heldout==1]))){
      warning("Heldout log_lik has been overwritten!!!!")
    }
    # We save here the log_lik of the fold k in the matrix:
    log_lik_heldout[heldout==1] <- log_lik[heldout==1]
  }
  return(log_lik_heldout)
}

#compute ELPD
kfold <- function(log_lik_heldout)  {
  library(matrixStats)
  logColMeansExp <- function(x) {
    # should be more stable than log(colMeans(exp(x)))
    S <- nrow(x)
    colLogSumExps(x) - log(S)
  }
  # See equation (20) of @VehtariEtAl2016
  pointwise <-  matrix(logColMeansExp(log_lik_heldout), ncol= 1)
  colnames(pointwise) <- "elpd"
  # See equation (21) of @VehtariEtAl2016
  elpd_kfold <- sum(pointwise)
  se_elpd_kfold <-  sqrt(ncol(log_lik_heldout) * var(pointwise))
  out <- list(
    pointwise = pointwise,
    elpd_kfold = elpd_kfold,
    se_elpd_kfold = se_elpd_kfold)
  #structure(out, class = "loo")
  return(out)
}


# Kick the stan code ------------------------------------------------------
# run the functions
ss <- stan_kfold(file="./model/linear_normal_regression_allowing_k_fold_cross_validation.stan",
                 data_l,
                 chains=4,
                 cores=2)
ee <- extract_log_lik_K(ss,holdout_10)
kk <- kfold(ee) 
#compare with official loo results
ll <- loo(ee)


# Comparing the model -----------------------------------------------------
# fit a too complex and a too simple model
X_comp <- cbind(X,runif(N,-2,2))
X_simp <- X[,1,drop=FALSE]

# new data
data_comp <- data_l
for(i in 1:10){
  data_comp[[i]]$X <- X_comp
  data_comp[[i]]$K <- 3
} 
data_simp <- data_l 
for(i in 1:10){
  data_simp[[i]]$X <- X_simp
  data_simp[[i]]$K <- 1
} 

# fit the new models
ss_comp <- stan_kfold(file="./model/linear_normal_regression_allowing_k_fold_cross_validation.stan",data_comp,chains=4,cores=2)
ss_simp <- stan_kfold(file="./model/linear_normal_regression_allowing_k_fold_cross_validation.stan",data_simp,chains=4,cores=2)

ee_comp <- extract_log_lik_K(ss_comp,holdout_10)
ee_simp <- extract_log_lik_K(ss_simp,holdout_10)

# compare the models
compare(loo(ee),loo(ee_comp),loo(ee_simp))



# Prediction --------------------------------------------------------------
# Choose best model in cross-validation
X_test <- cbind(rep(1,N),runif(N,-2,2))
y_test <- rnorm(N,X_test %*% c(1, 0.5),1)
ext_fit <- extract(ss[[1]]) # choose 1 chunk

# Extract posteriod distributions
sigma_post <- ext_fit$sigma
beta_post <- ext_fit$beta

# Function for simulating y based on new x
gen_quant_r <- function(x) {
  lin <- sample(beta_post[,1],size = nrow(x)) 
                  + x*sample(beta_post[,2], size = nrow(x))
  return(lin)
}

# Run the function on x_test
set.seed(56)
y_pred_r <- gen_quant_r(X_test)

# RMSE
sqrt(mean((y_pred_r - y_test)^2))
