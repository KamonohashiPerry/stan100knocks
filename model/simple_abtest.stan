data {
  int<lower=1> visitors_A;
  int<lower=0> conversions_A;
  int<lower=1> visitors_B;
  int<lower=0> conversions_B;
}

parameters {
  real eta_A;
  real eta_B;
}

transformed parameters {
  real<lower=0., upper=1.> pi_A = inv_logit(eta_A);
  real<lower=0., upper=1.> pi_B = inv_logit(eta_B);
}

model {
 eta_A ~ normal(0., 2.5);
 eta_B ~ normal(0., 2.5);
 conversions_A ~ binomial(visitors_A, pi_A);
 conversions_B ~ binomial(visitors_B, pi_B);
}

generated quantities {
  real<lower=-1., upper=1.> pi_diff;
  real eta_diff;
  real lift;
  
  pi_diff = pi_B - pi_A;
  eta_diff = eta_B - eta_A;
  lift = (pi_B - pi_A) / pi_B;
}
