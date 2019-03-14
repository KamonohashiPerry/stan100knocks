data {
	int<lower=1> T;
	int<lower=1> T_next;
	real<lower=0> Y[T];
}

parameters {
	real mu[T];
	real s[T];
	real<lower=0> s_mu;
	real<lower=0> s_s;
	real<lower=0> s_r;
}

model {
	for(t in 3:T)
		mu[t] ~ normal(2*mu[t-1] - mu[t-2] , s_mu);
	for(t in 3:T)
		s[t] ~ normal(-s[t-1]-s[t-2], s_s);

	for(t in 1:T)
		Y[t] ~ normal(mu[t]+s[t], s_r);
}

generated quantities {
	real mu_all[T+T_next];
	real s_all[T+T_next];
	real y_next[T_next];

	for (t in 1:T){
		mu_all[t] = mu[t];
		s_all[t] = s[t];
	}

	for (t in (T+1):(T+T_next)){
		mu_all[t] = normal_rng(2*mu_all[t-1] - mu_all[t-2], s_mu);
		s_all[t] = normal_rng(-s_all[t-1]-s_all[t-2], s_s);
		
	}

	for (t in 1:T_next)
		y_next[t] = normal_rng(mu_all[T+t]+s_all[T+t], s_r);
}
