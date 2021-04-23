data{
	int D; //n dimensions
	int N; //n individuals
	int R; //n items freelist
	real A[N]; //age of individuals
	int S[N];  //sex of individuals
	int Y_r[N,R]; //answers freelist
}//data

parameters{
  //individual parameters
	matrix[N,D] aK; // individual intercepts on knowledge
  vector<lower=0>[D] bA; // coefficient relating age to knowledge
	matrix[2,D] aS; //sex effect
	
	//item parameters
	//discrimination
	matrix<lower=0>[R,D] a_r;
	//difficulty
	matrix[R,D] b_r;

}//parameters

transformed parameters{
  matrix[N,D] K;
  for ( j in 1:D ) 
    for ( i in 1:N ) 
      K[i,j] = aK[i,j] + bA[j]*A[i] + aS[S[i],j]; 
}//transformed parameters

model{
  //priors for individual parameters
	to_vector(aK) ~ normal(0,1);
  for(i in 1:D) bA[i] ~ normal( 0 , 0.5 ) T[0,];
  to_vector(aS) ~ normal(0, 0.5);
  
	//priors for item parameters
	for(i in 1:D) for(j in 1:R)  a_r[j,i] ~ normal(0, 0.5) T[0,]; //value constrained above zero
	to_vector(b_r) ~ normal(0,1);



  //model
	//freelist
	for ( i in 1:N ) {
	  vector[R] p = rep_vector(0, R);
			for ( d in 1:D ) p = p + a_r[,d] .* (K[i,d] - b_r[,d]);
      target += bernoulli_logit_lpmf( Y_r[i,] | p );
		}//N
		
}//model
 generated quantities {
   vector [N * R ] log_lik;
{
   int k = 1;

    //freelist
		for ( i in 1:N ) {
      vector[R] p = rep_vector(0, R);
	    for ( d in 1:D ) p = p + a_r[,d] .* (K[i,d] - b_r[,d]);
      for (j in 1:R ) {
  			log_lik[k] = bernoulli_logit_lpmf( Y_r[ i, j] | p[j] );
   	  	k = k + 1;
   	  	} // L
      } // N
  }
}//generated quantities
