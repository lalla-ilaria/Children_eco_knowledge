data{
	//int N;      //number of children
	int M;      //number of trip
	int notZ[M];  //success of trip
	real R[M];  //returns
	real L[M];  //length of trip
	real K[M];  //individual knowledge
	real B[M];  //individual body
	}
parameters{
  real c; //parameter to move center of logit curve - in its absence all curves are forced to pass through x=1,y=0.5 and it distorts the curves. Makes sense?
  real<lower=0> z_l; //exponent for length of trip - prob success trip
  real<lower=0> z_k; //exponent for knowledge - prob success trip
  real<lower=0> z_b; //exponent for skill - prob success trip
	real<lower=0> r_l; //exponent for length of trip - amount per trip
	real<lower=0> r_k; //exponent for knowledge - amount per trip
	real<lower=0> r_b; //exponent for skill - amount per trip
	real<lower=0> sigma;
}
model{
  c ~ normal(1,0.5);
  z_l ~ lognormal(0,1);
  z_k ~ lognormal(0,1);
  z_b ~ lognormal(0,1);
  r_l ~ lognormal(0,1);
  r_k ~ lognormal(0,1);
  r_b ~ lognormal(0,1);
  sigma ~ exponential(1);
  for ( i in 1:M ) {
      real p =  1 - inv_logit( 2 * ( 1 - exp(  z_l * log( L [i] ) + 
                                               z_k * log( K [i] ) + 
                                               z_b * log( B [i] ))));
      notZ[i] ~ bernoulli( p ); 
      if ( notZ[i] > 0 ) {
         real m = r_l * log(L[i]) + 
                  r_k * log(K[i]) + 
                  r_b * log(B[i]) ;
			   R[i] ~ lognormal( exp(m) , sigma ); 
			   }
      }
}
