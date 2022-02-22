data{
	//int N;      //number of children
	int M;      //number of trip
	int notZ[M];  //success of trip
	real L[M];  //length of trip
	real K[M];  //individual knowledge
	real B[M];  //individual body
	}
parameters{
  real c;
  real<lower=0>  z_l;//exponent for length of trip - prob success trip
  real<lower=0>  z_k;//exponent for knowledge - prob success trip
  real<lower=0>  z_b;//exponent for skill - prob success trip
}
model{
  c ~ normal(1,0.5);
  z_l ~ lognormal(0,1);
  z_k ~ lognormal(0,1);
  z_b ~ lognormal(0,1);
  for ( i in 1:M ) {
      real p =  1 - inv_logit( 2 * ( c - exp(  z_l * log( L [i] ) + 
                                               z_k * log( K [i] ) + 
                                               z_b * log( B [i] ))));
      notZ[i] ~ bernoulli( p ); 
      }
}
