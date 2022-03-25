data{
	int N;      //number of children
	int M;      //number of trip
	int ID_ind[M];//id of forager/return
	real R[M];  //returns
	real L[M];  //length of trip
//	real K[N];  //individual knowledge
	real A[N];
	}
parameters{
  real<lower=0> alpha;
  real<lower=0> beta_a; //age effect
  real<lower=0> gamma_a; //age elasticity
	real<lower=0> lambda; //exponent for length trip
	real<lower=0> sigma;
}
transformed parameters{
  vector [N] phi;
  vector [M] psi;
  for(i in 1:N) phi[i]  = log(1-exp(-beta_a * A[i]  )) * gamma_a;
  for(i in 1:M) psi[i] = lambda * log (L[i]);

}
model{
  alpha ~ lognormal(0,1);
  beta_a ~lognormal(0, 1);
  gamma_a ~lognormal(0, 1);
  lambda ~ exponential(1);
  sigma ~ exponential(1);
  for ( i in 1:M ) {
         real m = alpha + phi[ID_ind[i]] + psi[i];
         R[i] ~ lognormal( exp(m) , sigma ); 
      }
}

  // phi[i,] <- log(1-exp(-beta_a[i] * AGE  )) * gamma_a[i]
  // psi[i] <-   lambda[i] * log (L[3])
  // R <- exp ( alpha[i] + phi[i,] + psi[i] + ((sigma[i]^2) /2))

