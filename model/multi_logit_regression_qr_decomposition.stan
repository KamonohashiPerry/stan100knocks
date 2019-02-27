data {
  int K;
  int N;
  int D;
  int y[N];
  matrix[N, D] x;
}

transformed data{
  matrix[K, K] A = diag_matrix(rep_vector(1,K));
  matrix[K, K-1] A_qr;
  for (i in 1:K-1) A[K,i] = -1;
  A[K,K] = 0;
  A_qr = qr_Q(A)[ , 1:(K-1)];
}

parameters {
  vector[K-1] beta_raw;
}

transformed parameters{
   vector[K] beta =  A_qr * beta_raw;
}


model {
  beta_raw ~ normal(0, inv(sqrt(1 - inv(K))));
}
