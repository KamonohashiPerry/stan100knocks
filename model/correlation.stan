data {
  int N; // sample size
  int<lower=1> L; // the number of variable
  vector[L] y[N];
}

parameters {
  vector[L] mu;
  vector<lower=0>[L] sigma;
  real<lower=-1,upper=1> rho;
}

transformed parameters {
  cov_matrix[L] Sig;
  for (i in 1:L) {
    for (j in 1:L) {
     if (i == j) {
       Sig[i,j] = sigma[i]^2;
     } 
     else {
       Sig[i,j] = rho*sigma[i]*sigma[j];
     }
    }
  }
}

model {
  mu ~ normal(0, 100);
  sigma ~ cauchy(0, 5);
  rho ~ uniform(-1, 1);
  y ~ multi_normal(mu, Sig);
}
