functions{
    real reducer( 
            int[] yy,
            int start , int end , 
            // data
            int[,] Y_q , 
            int[] A , int[] S , int D , 
            int Q ,
            // parameters to pass
            vector mA , matrix aK , 
            vector aK_sigma , matrix bA , 
            matrix delta_j , 
            matrix a_q , matrix b_q,
            vector c_q
          ) { 
    real lp = 0;
    for ( i in start:end ) {
			vector[Q] p_q = rep_vector(0, Q);
			vector[Q] logit_p_q;
			for ( d in 1:D ) {
        real K = mA[d] +                        
               aK[i,d] * aK_sigma[d] +                     
               bA[S[i], d] * sum (delta_j[ 1 : A[i], d ] ) ; 
        p_q = p_q + a_q[,d] .* (K - b_q[,d]);
        // log odds 3PL is log[(Exp[p]+c)/(1-c)]
        logit_p_q = log( exp(p_q) + c_q ) - log1m( c_q );
      }//d
		  lp = lp + bernoulli_logit_lpmf( Y_q[i,] | logit_p_q );
    }//i
    return lp;
  } 
}
data{
	int D; //n dimensions
	int N; //n individuals
	int Q; //n items questionnaire
	int O; //n ages
	int A[N]; //age of individuals
	int S[N]; //sex of individuals
	int Y_q[N,Q]; //answers questionnaire
  vector[O-1] alpha; //prior drichlet
}//data

parameters{
  //individual parameters
  vector[D] mA; //global intercept
	matrix[N,D] aK; // individual intercepts on knowledge
  matrix<lower=0>[2,D] bA; // coefficient relating age to knowledge
  simplex[O-1] delta [D]; //age specific effects
	//sigma individual parameters
	vector<lower=0>[D] aK_sigma;

	
	//item parameters
	//discrimination
	matrix<lower=0>[Q,D] a_q;
	//difficulty
	matrix[Q,D] b_q;
	//pseudoguessing
	vector<lower=0,upper=1>[Q] c_q;
	
}//parameters

transformed parameters{
  matrix[O,D] delta_j;
  for ( d in 1:D ) 
    delta_j[,d]  = append_row(0, delta[d]);
}//transformed parameters

model{
  //priors for individual parameters
  for(d in 1:D) mA[d] ~ normal( 0, 1)T[,0]; //global intercept
	to_vector(aK) ~ normal(0,1);
  for(d in 1:D) for(s in 1:2) bA[s,d] ~ normal( 0 , 1 ) T[0,];
  for(d in 1:D) delta[d] ~ dirichlet( alpha );
  //hyperpriors
	for(d in 1:D) aK_sigma[d] ~ exponential(1);
  
	//priors for item parameters
	//discrimination
	for(d in 1:D) for(j in 1:Q)  a_q[j,d] ~ normal(0, 1) T[0,]; //value constrained above zero
	//difficulty
	to_vector(b_q) ~ normal(0,2);
	//pseudoguessing
  c_q ~ beta(5,10);


  //model
	target += reduce_sum( reducer , A , 1 , 
      // data to pass
			Y_q , A , S , D , Q ,
      // parameters to pass
      mA , aK , aK_sigma , bA , delta_j , a_q , b_q , c_q );
}//model

 generated quantities {
   matrix[N,D] K;
   for ( d in 1:D ) 
      for ( i in 1:N ) 
      K[i,d] = mA[d] +                                           //global intercept - minimum value of knowledge
               aK[i,d] * aK_sigma[d] +                           //individual interecepts -absorbs residual variation   
               bA[S[i], d] * sum (delta_j[ 1 : A[i], d ] );     //effect of age - sex specific

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
