data{
	//int N;      //number of children
	int M;      //number of trip
	int S[M];  //success of trip
	int L[M];  //length of trip
	real K[M];  //individual knowledge
	real B[M];  //individual body
	}
parameters{
  real<lower=0> alpha;
  real<lower=0>  z_k;//exponent for knowledge - prob success trip
  real<lower=0>  z_b;//exponent for skill - prob success trip
}
model{
  alpha ~ normal(0,1) T[0,];
  z_k ~ lognormal (0,1) ;
  z_b ~ lognormal (0,1) ;
  for ( i in 1:M ) {
      real theta = 1 - 2 * ( 1 - inv_logit(   alpha *  K[i] ^ z_k *  
                          B[i] ^ z_b));
      S[i] ~ binomial ( L[i],theta); 
      }
}

// for binomial outcomes
      // real p =  2 * ( 1 - inv_logit ( alpha * L[i] ^ z_l *  
      //                                         K[i] ^ z_k *  
      //                                         B[i] ^ z_b ));
      // zero[i] ~ bernoulli( p ); 
// poisson outcomes
      // real l =   alpha *  L[i] ^ z_l *  
      //                        K[i] ^ z_k *  
      //                        B[i] ^ z_b;
      // S[i] ~ poisson( l ); 
