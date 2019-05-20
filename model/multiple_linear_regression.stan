data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] x;
  vector[N] y;
}

parameters {
  real<lower=0> sigma;
  real alpha;
  vector[K] beta; 
}

model {
  for (n in 1: N)
    y[n] ~ normal(x[n] * beta + alpha, sigma);
}
