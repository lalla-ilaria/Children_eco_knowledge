data{
	int N;      //number of children
	int M;      //number of trip
	int ID_ind[M];//id of forager/return
	int S[M];  //returns
	real L[M];  //length of trip
	real B[N];  //individual body
	real A[N];
	}
parameters{
  vector [N] id_v;
  real<lower=0> sigma_ind;
  real<lower=0> alpha;
  real<lower=0> beta_a; //age effect
  real<lower=0> gamma_a; //age elasticity
  real<lower=0> beta_b; //knowledge effect
  real<lower=0> gamma_b; //knowledge elasticity
	real<lower=0> lambda; //exponent for length trip
}
transformed parameters{
  vector [N] phi;
  vector [M] psi;
  for(i in 1:N) phi[i]  = exp(id_v[i] * sigma_ind) * 
                          (1-exp(-beta_a * A[i]  )) ^ gamma_a * 
                          (1-exp(-beta_b * B[i]  )) ^ gamma_b;
  for(i in 1:M) psi[i] =  (L[i])^lambda;

}
model{
  id_v ~ normal(0,1);
  sigma_ind ~ exponential(1);
  alpha ~ normal(0,1)T[0,];
  beta_a ~lognormal(0, 1);
  gamma_a ~lognormal(0, 1);
  beta_b ~lognormal(0, 1);
  gamma_b ~lognormal(0, 1);
  lambda ~ exponential(1);
  for ( i in 1:M ) {
         real p = 1 - exp (- alpha * phi[ID_ind[i]] * psi[i]);
         S[i] ~ bernoulli(p); 
      }
}

  // phi[i,] <- log(1-exp(-beta_a[i] * AGE  )) * gamma_a[i]
  // psi[i] <-   lambda[i] * log (L[3])
  // R <- exp ( alpha[i] + phi[i,] + psi[i] + ((sigma[i]^2) /2))

