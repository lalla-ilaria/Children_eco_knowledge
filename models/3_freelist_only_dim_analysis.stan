data{
	int D; //n dimensions
	int N; //n individuals
	int L; //n items freelist
	int Q; //n items questionnaire
	int R; //n items image recognition
	int O; //n ages
	int A[N]; //age of individuals
	int S[N]; //sex of individuals
	int Y_l[N,L]; //answers freelist
  vector[O-1] alpha; //prior drichlet
}//data

parameters{
  //individual parameters
  real mA; //global intercept
	matrix[N,D] aK; // individual intercepts on knowledge
  matrix<lower=0>[2,D] bA; // coefficient relating age to knowledge
  simplex[O-1] delta; //age specific effects

	
	//item parameters
	//discrimination
	matrix<lower=0>[L,D] a_l;
	//difficulty
	matrix[L,D] b_l;

}//parameters

transformed parameters{
  matrix[N,D] K;
  vector[O] delta_j;
  delta_j  = append_row(0, delta);
  for ( d in 1:D ) 
    for ( i in 1:N ) 
      K[i,d] = mA +                                           //global intercept - minimum value of knowledge
               aK[i,d] +                                      //individual interecepts -absorbs residual variation   
               bA[S[i], d] * sum (delta_j[ 1 : A[i] ] ) ;     //effect of age - sex specific

}//transformed parameters

model{
  //priors for individual parameters
  mA ~ normal( -6, 3)T[,0]; //global intercept
	to_vector(aK) ~ normal(0,2);
  for(d in 1:D) for(s in 1:2) bA[s,d] ~ normal( 0 , 5 ) T[0,];
  delta ~ dirichlet( alpha );
  
	//priors for item parameters
	//discrimination
	for(d in 1:D) for(j in 1:L)  a_l[j,d] ~ normal(0, 0.5) T[0,]; //value constrained above zero
	//difficulty
	to_vector(b_l) ~ normal(0,2);

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
