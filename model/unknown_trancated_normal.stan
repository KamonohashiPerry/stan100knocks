data {
  int<lower=1> N;
  real y[N];
}
parameters {
  real<upper = min(y)> L;
  real<lower = max(y)> U;
  real mu;
  real<lower=0> sigma;
}
model {
  L ~ normal(min(y), 2.5);
  U ~ normal(max(y), 2.5);
  for (n in 1:N)
    y[n] ~ normal(mu, sigma) T[L,U];
}
