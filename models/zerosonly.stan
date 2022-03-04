data{
	//int N;      //number of children
	int M;      //number of trip
	int zero[M];  //success of trip
	real L[M];  //length of trip
	real K[M];  //individual knowledge
	real B[M];  //individual body
	}
parameters{
  real alpha;
  real<lower=0>  z_l;//exponent for length of trip - prob success trip
  real<lower=0>  z_k;//exponent for knowledge - prob success trip
  real<lower=0>  z_b;//exponent for skill - prob success trip
}
model{
  alpha ~ normal(1,0.5);
  z_l ~ lognormal(0,1);
  z_k ~ lognormal(0,1);
  z_b ~ lognormal(0,1);
  for ( i in 1:M ) {
      real p =  2 * ( 1 - inv_logit ( alpha * L[i] ^ z_l *  
                                              K[i] ^ z_k *  
                                              B[i] ^ z_b ));
      zero[i] ~ bernoulli( p ); 
      }
}
