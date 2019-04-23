data{
	int ni; // the number of record
	int nj; // the number of item
	int nc; // the number of grade
	real D; // Constant
	int<lower=1,upper=10> y[ni,nj]; // data
}

parameters{
  vector<lower=0,upper=5>[nj] a;
	ordered[nc-1] ba[nj];
	vector<lower=-4,upper=4>[ni] theta;
}

transformed parameters{
	real b[nj,nc];
	vector<lower=0,upper=1>[nc-1] pa[ni,nj];
	simplex[nc] p[ni,nj];
	for (j in 1:nj){
		for (c in 1:nc){
			if (c ==1){
				b[j,c] = ba[j,c];
			}else if (c ==nc){
				b[j,c] = ba[j,c-1];
			}else{
				b[j,c] = (ba[j,c-1]+ba[j,c])/2;
			}
		}
	}
	for (i in 1:ni){
		for (j in 1:nj){
			for (c in 1:nc-1){
				pa[i,j,c] = 1/(1+exp(-D*a[j]*(theta[i] - ba[j,c])));
			}		
		}
	}
	for (i in 1:ni){
		for (j in 1:nj){
			for(c in 1:nc){
				if (c==1){
					p[i,j,c] = 1-pa[i,j,c];
				}else if(c==nc){
					p[i,j,c] = pa[i,j,c-1];
				}else{
					p[i,j,c] = pa[i,j,c-1] - pa[i,j,c];
				}
			}
		}
	}
}

model{
	for (i in 1:ni){
		theta[i] ~ normal(0,1);
		for (j in 1:nj){
			y[i,j] ~ categorical(p[i,j]);
		}
	}
	for (j in 1:nj){
		a[j] ~ lognormal(0,sqrt(0.5));
		for (c in 1:nc-1){
			ba[j,c] ~ normal(0,2);
		}
	}
}

generated quantities{
	real bg[nj,nc];
	bg = b;
}