data {
  int<lower=0> N;   // number of data items
  int<lower=0> K;   // number of predictors
  matrix[N, K] x;
  int<lower=0,upper=1> y[N];
}
parameters {
  real alpha;
  vector[K] beta;       // coefficients for predictors
}
model {
	for (n in 1:N)
	  y[n] ~ bernoulli(Phi_approx(alpha + x[n] * beta));
}
