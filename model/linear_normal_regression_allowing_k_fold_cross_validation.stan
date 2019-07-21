/*
Standard normal regression for any number of predictor variables
with weakly informative priors on the betas and on the standard deviation
*/
data {
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of predictor variables
  matrix[N, K] X; // the model matrix including intercept
  vector[N] y; // the response variable
  
  int<lower=0, upper=1> holdout[N];
  // index whether the observation should be held out (1) or used (0)
}

parameters {
  vector[K] beta; // the regression parameters
  real<lower=0> sigma;
}

model {
  vector[N] mu; // the linear predictor
  mu = X * beta; // the regresion
  // priors
  beta[1] ~ normal(0, 10);
  beta[2:K] ~ normal(0, 5);
  sigma ~ normal(0, 10);
  // likelihood holding out some data
  for(n in 1:N){
    if(holdout[n] == 0){
      target += normal_lpdf(y[n] | mu[n],sigma);
    }
  }
}

generated quantities{
  vector[N] log_lik;
  for (n in 1:N){
    log_lik[n] = normal_lpdf(y[n] | X[n,]*beta, sigma);
  }
}
