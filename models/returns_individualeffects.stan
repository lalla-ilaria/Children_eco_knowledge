data{
	int N;      //number of children
	int M;      //number of trip
	real R[M];  //returns
	real L[M];  //length of trip
	real K[M];  //individual knowledge
	real B[M];  //individual body
	int ID[M];
	}
parameters{
  real alpha;
  vector[N] alpha_ind;
  real<lower=0> r_l; //exponent for length of trip - amount per trip
	real<lower=0> r_k; //exponent for knowledge - amount per trip
	real<lower=0> r_b; //exponent for skill - amount per trip
	real<lower=0> sigma;
	real<lower=0> sigma_a;
}
model{
  alpha ~ normal (0,1);
  r_l ~ lognormal(0,2);
  r_k ~ lognormal(0,2);
  r_b ~ lognormal(0,2);
  sigma ~ exponential(1);
  sigma_a ~ exponential(1);
  alpha_ind ~ normal (alpha,sigma_a);
    for ( i in 1:M ) {
    real m = alpha_ind [ID[i]] + 
             r_l * log(L[i]) + r_k * log(K[i]) + r_b * log(B[i]) ;
		R[i] ~ lognormal( exp(m) , sigma ); 
      }
}
