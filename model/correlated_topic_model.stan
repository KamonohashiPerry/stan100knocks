data {
  int<lower=1> K;
  int<lower=1> M;
  int<lower=1> V;
  int<lower=1> N;
  int<lower=1,upper=V> W[N];
  int<lower=1,upper=N> Offset[M, 2]; // range of word index per doc
  vector[K] mu; // topic mean
  cov_matrix[K] Sigma; // topic covatiance
}

parameters {
  vector[K] eta[M];
  simplex[V] phi[K];
}

transformed parameters {
  simplex[K] theta[M];
  for (m in 1:M)
    theta[m] = softmax(eta[m]);
}

model {
  // prior
  for(m in 1:M)
    eta[m] ~ multi_normal(mu, Sigma);
    
  // likelihood
  for (m in 1:M) {
    for (n in Offset[m, 1]:Offset[m,2]){
      real gamma[K];
      for (k in 1:K)
        gamma[k] = log(theta[m,k]) + log(phi[k,W[n]]);
      target += log_sum_exp(gamma);
    }      
  }
}
