data{
	//int N;      //number of children
	int M;      //number of trip
	int Z[M];  //success of trip
	real R[M];  //returns
	real L[M];  //length of trip
	real K[M];  //individual knowledge
	real B[M];  //individual body
	}
parameters{
  real<lower=0>  z_l;//exponent for length of bout - prob success bout
  real<lower=0>  z_k;//exponent for knowledge - prob success bout
  real<lower=0>  z_b;//exponent for skill - prob success bout
	real<lower=0,upper=1> r_l; //exponent for length of bout - amount per bout
	real<lower=0,upper=1> r_k; //exponent for knowledge - amount per bout
	real<lower=0,upper=1> r_b; //exponent for skill - amount per bout
	real<lower=0> sigma;
}
model{
  z_l ~ lognormal(0,1);
  z_k ~ lognormal(0,1);
  z_b ~ lognormal(0,1);
  r_l ~ uniform(0,1);
  r_k ~ uniform(0,1);
  r_b ~ uniform(0,1);
  sigma ~ exponential(1);
  for ( i in 1:M ) {
      real p = 1 - exp( - z_l * L [i] - z_k * K [i] - z_b * B [i] ); //logistic regression <- plus log all terms
      Z[i] ~ bernoulli( p );
      if ( Z[i] > 0 ) {
         real m = r_l * log(L[i]) + r_k * log(K[i]) + r_b * log(B[i]) ;
			   R[i] ~ lognormal( exp(m) , sigma ); //exp(m)? mean of lognormal is exp(m + sigma^2/2) <- see wikipedia
			   }
      }
}
