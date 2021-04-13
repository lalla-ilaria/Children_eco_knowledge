data{
	int D; //n dimensions
	int N; //n individuals
	int L; //n items freelist
	int Y_l[N,L]; //answers freelist
}//data

parameters{
  //individual parameters
	matrix[N,D] aK; // individual intercepts on knowledge

	//item parameters
	//discrimination
	matrix<lower=0>[L,D] a_l;
	//difficulty
	matrix[L,D] b_l;

}//parameters

transformed parameters{
  matrix[N,D] K;
  for ( j in 1:D ) 
    for ( i in 1:N ) 
      K[i,j] = aK[i,j] ; 
}//transformed parameters

model{
  //priors for individual parameters
	to_vector(aK) ~ normal(0,1);

	//priors for item parameters
	to_vector(a_l) ~ normal(0, 0.5); //value constrained above zero
	to_vector(b_l) ~ normal(0,1);


  //model
	//freelist
	for ( i in 1:N ) {
	  vector[L] p = rep_vector(0, L);
			for ( d in 1:D ) p = p + a_l[,d] .* (K[i,d] - b_l[,d]);
      target += bernoulli_logit_lpmf( Y_l[i,] | p );
		}//N

}//model

 generated quantities {
   vector [N * L ] log_lik;
{		
   int k = 1;
   
    //freelist
		for ( i in 1:N ) {
      vector[L] p = rep_vector(0, L);
	    for ( d in 1:D ) p = p + a_l[,d] .* (K[i,d] - b_l[,d]);
      for (j in 1:L ) {
  			log_lik[k] = bernoulli_logit_lpmf( Y_l[ i, j] | p[j] );
   	  	k = k + 1;
   	  	} // L
      } // N
  }
}//generated quantities
