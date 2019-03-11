data {
  int<lower=1> K;
  int<lower=0> N;
  matrix[N, K] x;
  vector[N] y;

  int<lower=0> N_new;
  matrix[N_new, K] x_new;
}
parameters {
  vector[K] beta;
  real<lower=0> sigma;

  vector[N_new] y_new;                  // predictions
}
model {
  y ~ normal(x * beta, sigma);          // observed model

  y_new ~ normal(x_new * beta, sigma);  // prediction model
}
