data{
	int N;      //number of children
	int M;      //number of trip
	real R[M];  //returns
	real L[M];  //length of trip
	real K[M];  //individual knowledge
	real B[M];  //individual body
	real A[N];  //individual age
	int ID[M];
	}
parameters{
  real alpha;
  real r_a;
  real<lower=0> r_l; //exponent for length of trip - amount per trip
	real<lower=0> r_k; //exponent for knowledge - amount per trip
	real<lower=0> r_b; //exponent for skill - amount per trip
	real<lower=0> sigma;
	real<lower=0> sigma_a;
}
transformed parameters{
  vector[N] alpha_ind;
  for (i in 1:N){
    alpha_ind[i] = alpha + r_a * A[i];
  }
}
model{
  alpha ~ normal (0,1);
  r_a ~ normal (0,1);
  r_l ~ lognormal(0,2);
  r_k ~ lognormal(0,2);
  r_b ~ lognormal(0,2);
  sigma ~ exponential(1);
  sigma_a ~ exponential(1);
  //alpha_ind ~ normal (alpha,sigma_a);
  // for (j in 1:N){
  //   real mu_a = alpha + r_a * A[j];
  //   alpha_ind[j] ~ normal (mu_a,sigma_a)
  // }
    for ( i in 1:M ) {
    real m;
    m = alpha_ind [ID[i]] + 
             r_l * log(L[i]) + r_k * log(K[i]) + r_b * log(B[i]) ;
		R[i] ~ lognormal( exp(m) , sigma ); 
      }
}
