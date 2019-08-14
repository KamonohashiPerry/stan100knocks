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
  real mu_alpha; // the mean intercept
  real mu_beta; // the mean slope

  real<lower=0> sigmasq_y;
  real<lower=0> sigmasq_alpha;
  real<lower=0> sigmasq_beta;
}

transformed parameters {
  real<lower=0> sigma_alpha; // sd of intercept distribution
  real<lower=0> sigma_beta; // sd of slope distribution

  sigma_alpha = sqrt(sigmasq_alpha);
  sigma_beta = sqrt(sigmasq_beta);
}

model {
  vector[N] mu; // the linear predictor
  mu = X * beta; // the regresion
  // priors
  mu_alpha ~ normal(0, 100); // non-informative prior
  mu_beta ~ normal(0, 100); // non-informative prior
  sigmasq_y ~ inv_gamma(0.001, 0.001); // conjugate prior of normal
  sigmasq_alpha ~ inv_gamma(0.001, 0.001); // conjugate prior of normal
  sigmasq_beta ~ inv_gamma(0.001, 0.001); // conjugate prior of normal
  alpha ~ normal(mu_alpha, sigma_alpha); // all intercepts are normal 
  
  beta[1:K] ~ normal(mu_beta, sigma_beta);

  // likelihood holding out some data
  for(n in 1:N){
    if(holdout[n] == 0){
      target += bernoulli_lpmf( y[n] | inv_logit(alpha + mu[n]));
    }
  }
}
