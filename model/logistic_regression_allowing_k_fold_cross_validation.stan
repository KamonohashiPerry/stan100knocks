data {
  int<lower=0> N;   // number of data items
  int<lower=0> K;   // number of predictors
  matrix[N, K] X;
  int<lower=0,upper=1> y[N];
  
  int<lower=0, upper=1> holdout[N];
  // index whether the observation should be held out (1) or used (0)
}

parameters {
  real alpha;
  vector[K] beta;       // coefficients for predictors
}

model {
  vector[N] mu; // the linear predictor
  mu = X * beta; // the regresion
  // priors
  alpha ~ normal(0, 10);
  beta[1:K] ~ normal(0, 5);

  // likelihood holding out some data
  for(n in 1:N){
    if(holdout[n] == 0){
      target += bernoulli_lpmf( y[n] | inv_logit(alpha + mu[n]));
    }
  }
}
