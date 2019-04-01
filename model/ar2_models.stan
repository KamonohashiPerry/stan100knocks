data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real alpha;
  real beta;
  real gamma;
  real<lower=0> sigma;
}
model {
  for (n in 3:N)
    y[n] ~ normal(alpha + beta*y[n-1] + gamma*y[n-2], sigma);
}
