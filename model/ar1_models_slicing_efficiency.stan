data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  y[2:N] ~ normal(alpha + beta * y[1:(N - 1)], sigma);
}
