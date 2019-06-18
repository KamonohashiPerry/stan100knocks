data {
  int N;
  int N_tanks;
  int tank_id[N];
  int density[N];
  int surv[N];
}
parameters {
  vector[N_tanks] a_tank;
}
model {
  vector[N] p;
  target += normal_lpdf(a_tank | 0, 5);
  for(i in 1:N) p[i] = inv_logit( a_tank[tank_id[i]] );
  target += binomial_lpmf(surv | density, p);
}
generated quantities {
  vector[N] log_lik;
  {
  vector[N] p;
  for(i in 1:N) {
    p[i] = inv_logit( a_tank[tank_id[i]] );
    log_lik[i] = binomial_lpmf(surv[i] | density[i], p[i]);
  }
  }
}
