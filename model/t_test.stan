data {
  int N;
  real y[N];
  real x[N];
}

parameters {
  real mu_y;
  real<lower=0> sigma_y;
  real mu_x;
  real<lower=0> sigma_x;
}

model {
  mu_y ~ normal(0, 100);
  sigma_y ~ cauchy(0, 5);
  mu_x ~ normal(0, 100);
  sigma_x ~ cauchy(0, 5);
}

generated quantities {
  real diff;
  diff = mu_x - mu_y;
}
