data{
 int<lower = 1> N;
 vector[N] x;
 vector[N] y; 

 int<lower = 1> N2;
 vector[N2] x2;
}

transformed data{
 vector[N] mu;
 for(i in 1:N)
   mu[i] = 0;
}

parameters{
 real<lower = 0> eta_sq;
 real<lower = 0> rho_sq;
 real<lower = 0> sigma_sq;
}

transformed parameters{
 matrix[N, N] Cov;

 for(i in 1:(N-1)){
   for(j in (i+1):N){
     Cov[i, j] = eta_sq * exp(- rho_sq * pow(x[i] - x[j], 2));
     Cov[j, i] = Cov[i, j];
   }
 }

 for(k in 1:N)
   Cov[k, k] = sigma_sq;
}

model{
 y ~ multi_normal(mu, Cov);

 eta_sq ~ cauchy(0, 5);
 rho_sq ~ cauchy(0, 5);
 sigma_sq ~ cauchy(0, 5); 
}

generated quantities{
  vector[N2] y2;
  vector[N2] mu2;
  matrix[N2, N2] Cov2;
  matrix[N, N2] K;
  matrix[N2, N2] Sigma;
  matrix[N2, N] K_t_Cov;

// 式(6)
  for (i in 1:N)
    for (j in 1:N2)
      K[i, j] = eta_sq * exp(-rho_sq * pow(x[i] - x2[j],2));

// 式(7)
  for(i in 1:(N2-1)){
    for(j in (i+1):N2){
      Sigma[i, j] = eta_sq * exp(-rho_sq * pow(x2[i] - x2[j], 2));
      Sigma[j, i] = Sigma[i,j];
    }
  }

  for(k in 1:N2)
    Sigma[k, k] = sigma_sq;

// 式(8)-1
  K_t_Cov = K' / Cov;  
  mu2 = K_t_Cov * y;

// 式(8)-2
  Cov2 = Sigma - K_t_Cov * K;

  for(i in 1:N2)
    for(j in (i+1):N2)
      Cov2[i, j] = Cov2[j, i];

// 式(3)
  y2 = multi_normal_rng(mu2, Cov2);
}
