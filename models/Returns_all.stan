data{
	int N;      //number of children
	int M;      //number of trip
	int ID_i[M];//id of forager/return
	real R[M];  //returns
	real L[M];  //length of trip
	real K[N];  //individual knowledge
	real B[N];  //individual body
	real A[N];
	}
parameters{
  vector [N] iota;
  real<lower=0> sigma_i;
  real<lower=0> alpha;
  real<lower=0> beta; //age effect
  real<lower=0> gamma; //age elasticity
  real zeta_k; //knowledge elasticity
  real eta_b; //knowledge elasticity
  real xi; //exponent for length trip
	real<lower=0> sigma;
}
transformed parameters{
  vector [N] phi;
  vector [M] psi;
  for(i in 1:N) phi[i]  = exp (iota[i] * sigma_i) * ( 
                          (1-exp(-beta * A[i]  )) ^ gamma * 
                          K[i] ^ zeta_k * 
                          B[i] ^ eta_b);
  for(i in 1:M) psi[i] =  L[i] ^ xi;

}
model{
  iota ~ normal(0,1);
  sigma_i ~ exponential(1);
  alpha ~ normal(0,1)T[0,];
  beta ~ lognormal(0, 1);
  gamma~ lognormal(0, 1);
  zeta_k~ normal(0, 1);
  eta_b ~ normal(0, 1);
  xi ~ normal(0, 1);
  sigma ~ exponential(1);
  for ( i in 1:M ) {
         real m = log( alpha * phi[ID_i[i]] * psi[i]);
         R[i] ~ lognormal( exp(m) , sigma ); 
      }
}