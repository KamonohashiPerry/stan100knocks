data {
  int<lower=0> N;
  int<lower=0> y[N];
}

parameters {
    real<lower=0> lambda;
}

model {
  for (n in 1:N){
      target += poisson_lpmf(y[n] | lambda);
  }
}
