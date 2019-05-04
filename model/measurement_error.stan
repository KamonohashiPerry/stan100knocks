data {
  int<lower=0>N;
  vector[N] y;
  real x_meas[N];
  real<lower=0> tau;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
  real x[N];
  real mu_x;
  real sigma_x;
}

model {
  for (n in 1:N){
    x ~ normal(mu_x, sigma_x);
    x_meas[n] ~ normal(x, tau);
    y[n] ~ normal(alpha + beta*x[n], sigma);
    alpha ~ normal(0, 10);
    beta ~ normal(0, 10);
    sigma ~ cauchy(0, 5);
  }
}
