functions{
    real reducer( 
            int[] yy,
            int start , int end , 
            // data
            int[,] Y_l , int[,] Y_q , int[,] Y_r , 
            int[] A , int[] S , int[] HD , int D , 
            int L , int Q ,int R ,
            // parameters to pass
            vector mA , matrix aK , 
            vector aK_sigma , matrix bA , bHD ,
            matrix delta_j , 
            matrix a_l , matrix b_l,
            matrix a_q , matrix b_q,
            matrix a_r , matrix b_r,
            vector c_q
          ) { 
    real lp = 0;
    for ( i in start:end ) {
      vector[L] p_l = rep_vector(0, L);
			vector[Q] p_q = rep_vector(0, Q);
			vector[Q] logit_p_q;
			vector[R] p_r = rep_vector(0, R);
			for ( d in 1:D ) {
        real K = mA[d] +                        
               aK[i,d] * aK_sigma[d] +                     
               bA[S[i], d] * sum (delta_j[ 1 : A[i], d ] ) +
               bHD[d] * HD[i]; 
        p_l = p_l + a_l[,d] .* (K - b_l[,d]);
        p_q = p_q + a_q[,d] .* (K - b_q[,d]);
        // log odds 3PL is log[(Exp[p]+c)/(1-c)]
        logit_p_q = log( exp(p_q) + c_q ) - log1m( c_q );
        p_r = p_r + a_r[,d] .* (K - b_r[,d]);
      }//d
		  lp = lp + bernoulli_logit_lpmf( Y_l[i,] | p_l );
      lp = lp + bernoulli_logit_lpmf( Y_q[i,] | logit_p_q );
      lp = lp + bernoulli_logit_lpmf( Y_r[i,] | p_r );
    }//i
    return lp;
  } 
}
data{
	int D; //n dimensions
	int N; //n individuals
	int L; //n items freelist
	int Q; //n items questionnaire
	int R; //n items image recognition
	int O; //n ages
	int A[N]; //age of individuals
	int S[N]; //sex of individuals
	real HD[N];//distance from research station
	int Y_l[N,L]; //answers freelist
  int Y_q[N,Q]; //answers questionnaire
  int Y_r[N,R]; //answers image recognition
  vector[O-1] alpha; //prior drichlet
}//data

parameters{
  //individual parameters
  vector[D] mA; //global intercept
	matrix[N,D] aK; // individual intercepts on knowledge
  matrix<lower=0>[2,D] bA; // coefficient relating age to knowledge
  vector[D] bHD; //effect of distance from research station
  simplex[O-1] delta [D]; //age specific effects
	//sigma individual parameters
	vector<lower=0>[D] aK_sigma;

	
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
  matrix[O,D] delta_j;
  for ( d in 1:D ) 
    delta_j[,d]  = append_row(0, delta[d]);
}//transformed parameters

model{
  //priors for individual parameters
  for(d in 1:D) mA[d] ~ normal( -5, 3)T[,0]; //global intercept
	to_vector(aK) ~ normal(0,1);
  for(d in 1:D) for(s in 1:2) bA[s,d] ~ normal( 3 , 2 ) T[0,];
  bHD ~ normal( 0 , 1 )
  for(d in 1:D) delta[d] ~ dirichlet( alpha );
  //hyperpriors
	for(d in 1:D) aK_sigma[d] ~ exponential(1);
  
	//priors for item parameters
	//discrimination
	for(d in 1:D) for(j in 1:L)  a_l[j,d] ~ normal(0, 1) T[0,]; //value constrained above zero
	for(d in 1:D) for(j in 1:Q)  a_q[j,d] ~ normal(0, 1) T[0,]; //value constrained above zero
	for(d in 1:D) for(j in 1:R)  a_r[j,d] ~ normal(0, 1) T[0,]; //value constrained above zero
	//difficulty
	to_vector(b_q) ~ normal(0,2);
	to_vector(b_l) ~ normal(0,2);
	to_vector(b_r) ~ normal(0,2);
	//pseudoguessing
  c_q ~ beta(5,10);


  //model
	target += reduce_sum( reducer , A , 1 , 
      // data to pass
			Y_l , Y_q , Y_r , A , S , HD ,  D , L , Q , R ,
      // parameters to pass
      mA , aK , aK_sigma , bA , bHD , delta_j , a_l , b_l , a_q , b_q , a_r , b_r, c_q );
}//model

 generated quantities {
   matrix[N,D] K;
   for ( d in 1:D ) 
      for ( i in 1:N ) 
      K[i,d] = mA[d] +                                           //global intercept - minimum value of knowledge
               aK[i,d] * aK_sigma[d] +                           //individual interecepts -absorbs residual variation   
               bA[S[i], d] * sum (delta_j[ 1 : A[i], d ] ) +     //effect of age - sex specific
               bHD[d] * HD[i];                                   //effect of distance from research station

   vector [N * L + N * Q + N * R ] log_lik;
   vector [N * L ] log_lik_l;
   vector [N * Q ] log_lik_q;
   vector [N * R ] log_lik_r;
  {
    int k = 1;
    int l = 1;
    int q = 1;
    int r = 1;
      
    //freelist
		for ( i in 1:N ) {
      vector[L] p = rep_vector(0, L);
	    for ( d in 1:D ) p = p + a_l[,d] .* (K[i,d] - b_l[,d]);
      for (j in 1:L ) {
  			log_lik[k] = bernoulli_logit_lpmf( Y_l[ i, j] | p[j] );
  			log_lik_l[l] = log_lik[k];
   	  	k = k + 1;
   	  	l = l + 1;
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
   			log_lik_q[q] = log_lik[k];
  	  	k = k + 1;
   	  	q = q + 1;
   	  	} // L
      } // N
    //image recognition
		for ( i in 1:N ) {
      vector[R] p = rep_vector(0, R);
	    for ( d in 1:D ) p = p + a_r[,d] .* (K[i,d] - b_r[,d]);
      for (j in 1:R ) {
  			log_lik[k] = bernoulli_logit_lpmf( Y_r[ i, j] | p[j] );
    		log_lik_r[r] = log_lik[k];
  	  	k = k + 1;
   	  	r = r + 1;
   	  	} // L
      } // N
    }
}//generated quantities
