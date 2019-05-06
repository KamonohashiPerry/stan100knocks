data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Time[N];
  int<lower=0,upper=1> Cens[N];
}

parameters {
  vector<upper=0>[T] log_hazard;
  real<lower=0> s_lh;
}

transformed parameters {
  vector[T+1] log_F;
  log_F[1] = 0;
  for (t in 2:(T+1))
    log_F[t] = log_F[t-1] + log1m_exp(log_hazard[t-1]);
  
}

model {
  for (t in 3:T)
    log_hazard[t] ~ normal(2*log_hazard[t-1] - log_hazard[t-2], s_lh);
  for (n in 1:N)
    increment_log_prob(
      Cens[n]==1 ? log_hazard[Time[n]] + log_F[Time[n]] : log_F[Time[n] + 1]);
}

generated quantities {
  vector[T] hazard;
  hazard = exp(log_hazard);
}
