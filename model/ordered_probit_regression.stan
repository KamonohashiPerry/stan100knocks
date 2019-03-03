data{
  int<lower=1> N; // number of obs
  int<lower=3> J; // number of categories
  int<lower=2> K; // num of predictors
  int<lower=0,upper=10> y[N]; // outcome var 
  matrix[N, K] x; // predictor vars 
}

parameters{
  ordered[J-1] tau; // thresholds
  vector[K] beta; // beta coefficients 
}

model{
  vector[J] theta;
  vector[N] xB;
  beta ~ normal(0, 10);
  xB = x*beta;
  for(n in 1:N){
    theta[1] = 1 - Phi(xB[n]-tau[1]);
    for(j in 2:J-1)
      theta[j] = Phi(xB[n]-tau[j-1]) - Phi(xB[n]-tau[j]);
    theta[J] = Phi(xB[n] - tau[J-1]);
    y[n] ~ categorical(theta);
  }
}
