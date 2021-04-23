data{
	int D; //n dimensions
	int N; //n individuals
	int Q; //n items freelist
	real A[N]; //age of individuals
	int S[N];  //sex of individuals
	int Y_q[N,Q]; //answers freelist
}//data

parameters{
  //individual parameters
	matrix[N,D] aK; // individual intercepts on knowledge
  vector<lower=0>[D] bA; // coefficient relating age to knowledge
	matrix[2,D] aS; //sex effect
	
	//item parameters
	//discrimination
	matrix<lower=0>[Q,D] a_q;
	//difficulty
	matrix[Q,D] b_q;
	//pseudoguessing
	vector<lower=0,upper=1>[Q] c_q;
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
	for(i in 1:D) for(j in 1:Q)  a_q[j,i] ~ normal(0, 0.5) T[0,]; //value constrained above zero
	to_vector(b_q) ~ normal(0,1);
  c_q ~ beta(5,10);


  //model
	//questions
	for ( i in 1:N ) {
	  vector[Q] p = rep_vector(0, Q);
    vector[Q] logit_p;
			for ( d in 1:D ) p = p + a_q[,d] .* (K[i,d] - b_q[,d]);
      // log odds 3PL is log[(Exp[p]+c)/(1-c)]
      logit_p = log( exp(p) + c_q ) - log1m( c_q );
      target += bernoulli_logit_lpmf( Y_q[i,] | logit_p );
		}//N
		
}//model
 generated quantities {
   vector [N * Q ] log_lik;
{
   int k = 1;

    //questions
	  for ( i in 1:N ) {
	  vector[Q] p = rep_vector(0, Q);
    vector[Q] logit_p;
			for ( d in 1:D ) p = p + a_q[,d] .* (K[i,d] - b_q[,d]);
      // log odds 3PL is log[(Exp[p]+c)/(1-c)]
      logit_p = log( exp(p) + c_q ) - log1m( c_q );
      for (j in 1:Q ) {
  			log_lik[k] = bernoulli_logit_lpmf( Y_q[ i, j] | p[j] );
   	  	k = k + 1;
   	  	} // L
      } // N
  }
}//generated quantities
