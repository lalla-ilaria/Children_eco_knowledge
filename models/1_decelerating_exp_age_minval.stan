data{
	int D; //n dimensions
	int N; //n individuals
	int L; //n items freelist
	int Q; //n items questionnaire
	int R; //n items image recognition
	int S[N];  //sex of individuals 
	real A[N]; //age of individuals
	int Y_l[N,L]; //answers freelist
  int Y_q[N,Q]; //answers questionnaire
  int Y_r[N,R]; //answers image recognition
}//data

parameters{
  //individual parameters
	matrix[N,D] aK; // individual intercepts on knowledge
	matrix<lower=0>[2,D] aA; //intercept for sex
  matrix<lower=0>[2,D] bA; // coefficient relating age to knowledge
	real<upper=0> mA;
	
	//item parameters
	//discrimination
	matrix<lower=0>[L,D] a_l;
	matrix<lower=0>[Q,D] a_q;
	matrix<lower=0>[R,D] a_r;
	//difficulty
	matrix[L,D] b_l;
	matrix[Q,D] b_q;
	matrix[R,D] b_r;
	//pseudoguessing
	vector<lower=0,upper=1>[Q] c_q;
	
}//parameters

transformed parameters{
  matrix[N,D] K;
  for ( j in 1:D ) 
    for ( i in 1:N ) 
      K[i,j] = aK[i,j] + mA + aA[S[i],j] * ( 1 - exp(-bA[S[i],j]*A[i])); 
}//transformed parameters

model{
  //priors for individual parameters
	to_vector(aK) ~ normal(0,1);
	for(i in 1:D) for ( s in 1:2 ) aA[s,i] ~ normal( 1, 0.5)T[0,];
  for(i in 1:D) for ( s in 1:2 ) bA[s,i] ~ normal( 0.5, 0.5 )T[0,] ;
  mA ~ normal(-2, 3)T[,0];
  
	//priors for item parameters
	for(i in 1:D) for(j in 1:L)  a_l[j,i] ~ normal(0, 0.5) T[0,]; //value constrained above zero
	for(i in 1:D) for(j in 1:Q)  a_q[j,i] ~ normal(0, 0.5) T[0,]; //value constrained above zero
	for(i in 1:D) for(j in 1:R)  a_r[j,i] ~ normal(0, 0.5) T[0,]; //value constrained above zero
	to_vector(b_q) ~ normal(0,1);
	to_vector(b_l) ~ normal(0,1);
	to_vector(b_r) ~ normal(0,1);
  c_q ~ beta(5,10);


  //model
	//freelist
	for ( i in 1:N ) {
	  vector[L] p = rep_vector(0, L);
			for ( d in 1:D ) p = p + a_l[,d] .* (K[i,d] - b_l[,d]);
      target += bernoulli_logit_lpmf( Y_l[i,] | p );
		}//N
		
	//questions
	for ( i in 1:N ) {
	  vector[Q] p = rep_vector(0, Q);
    vector[Q] logit_p;
			for ( d in 1:D ) p = p + a_q[,d] .* (K[i,d] - b_q[,d]);
      // log odds 3PL is log[(Exp[p]+c)/(1-c)]
      logit_p = log( exp(p) + c_q ) - log1m( c_q );
      target += bernoulli_logit_lpmf( Y_q[i,] | logit_p );
		}//N
	
	//image recognition
	for ( i in 1:N ) {
	  vector[R] p = rep_vector(0, R);
			for ( d in 1:D ) p = p + a_r[,d] .* (K[i,d] - b_r[,d]);
      target += bernoulli_logit_lpmf( Y_r[i,] | p );
	}//N
}//model
 generated quantities {
   vector [N * L + N * Q + N * R ] log_lik;
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
    //image recognition
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
