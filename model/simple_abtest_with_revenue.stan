data {
  int<lower=2> outcomes;
  int n_A[outcomes];
  int n_B[outcomes];
  vector[outcomes] payoffs;
  vector[outcomes] a_A;
  vector[outcomes] a_B;
}

parameters {
  simplex[outcomes] theta_A;
  simplex[outcomes] theta_B;
}

model {
  theta_A ~ dirichlet(a_A);
  theta_B ~ dirichlet(a_B);
  
  n_A ~ multinomial(theta_A);
  n_B ~ multinomial(theta_B);
}

generated quantities {
  vector[outcomes] theta_diff;
  real revenue_A;
  real revenue_B;
  real revenue_diff;
  theta_diff = theta_B - theta_A;
  revenue_A = dot_product(theta_B, payoffs);
  revenue_B = dot_product(theta_A, payoffs);
  revenue_diff = revenue_B - revenue_A;
}
