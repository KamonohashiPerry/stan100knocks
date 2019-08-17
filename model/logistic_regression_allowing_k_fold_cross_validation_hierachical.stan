data {
  int<lower=0> N;   // number of data items
  int<lower=0> K;   // number of predictors
  int<lower=0> M;   // number of group
  row_vector[K] X[N];
  int<lower=0,upper=1> y[N];
  int<lower=0,upper=M> citric_acid_group[N];
  int<lower=0, upper=1> holdout[N];
  // index whether the observation should be held out (1) or used (0)
}

parameters {
  real mu[K];
  real<lower=0> sigma[K];
  vector[K] beta[M];
}

model {
  for (k in 1:K) {
    mu[k] ~ normal(0, 100);
    sigma[k] ~ inv_gamma(1, 1);

    for (m in 1:M)
      beta[m,k] ~ normal(mu[k], sigma[k]);
  }
  for (n in 1:N){
    if(holdout[n] == 0){
      target += bernoulli_lpmf( y[n] | inv_logit(X[n] * beta[citric_acid_group[n]]));
  }
 }
}
