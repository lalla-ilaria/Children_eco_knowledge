functions{
    real reducer( 
            int[] yy,
            int start , int end , 
            // data
            int[,] Y_l , int[,] Y_r , 
            int[] A , int[] S , int D , 
            int L , int R ,
            // parameters to pass
            vector mA , matrix aK , 
            vector aK_sigma , matrix bA , 
            matrix delta_j , 
            matrix a_l , matrix b_l,
            matrix a_r , matrix b_r
          ) { 
    real lp = 0;
    for ( i in start:end ) {
      vector[L] p = rep_vector(0, L);
			for ( d in 1:D ) {
        real K = mA[d] +                        
               aK[i,d] * aK_sigma[d] +                     
               bA[S[i], d] * sum (delta_j[ 1 : A[i], d ] ) ; 
        p = p + a_l[,d] .* (K - b_l[,d]);
      }//d
		  lp = lp + bernoulli_logit_lpmf( Y_l[i,] | p );
    }//i
      for ( i in start:end ) {
      vector[R] p = rep_vector(0, R);
			for ( d in 1:D ) {
        real K = mA[d] +                        
               aK[i,d] * aK_sigma[d] +                     
               bA[S[i], d] * sum (delta_j[ 1 : A[i], d ] ); 
        p = p + a_r[,d] .* (K - b_r[,d]);
      }//d
		  lp = lp + bernoulli_logit_lpmf( Y_r[i,] | p );
    }//i
    return lp;
  } 
}
data{
	int D; //n dimensions
	int N; //n individuals
	int L; //n items freelist
	int R; //n items image recognition
	int O; //n ages
	int A[N]; //age of individuals
	int S[N]; //sex of individuals
	int Y_l[N,L]; //answers freelist
  int Y_r[N,R]; //answers image recognition
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
	matrix<lower=0>[L,D] a_l;
	matrix<lower=0>[R,D] a_r;
	//difficulty
	matrix[L,D] b_l;
	matrix[R,D] b_r;

}//parameters

transformed parameters{
  matrix[O,D] delta_j;
  for ( d in 1:D ) 
    delta_j[,d]  = append_row(0, delta[d]);
}//transformed parameters

model{
  //priors for individual parameters
  for(d in 1:D) mA[d] ~ normal( 0, 3)T[,0]; //global intercept
	to_vector(aK) ~ normal(0,1);
  for(d in 1:D) for(s in 1:2) bA[s,d] ~ normal( 0 , 3 ) T[0,];
  for(d in 1:D) delta[d] ~ dirichlet( alpha );
  //hyperpriors
	for(d in 1:D) aK_sigma[d] ~ exponential(1);
  
	//priors for item parameters
	//discrimination
	for(d in 1:D) for(j in 1:L)  a_l[j,d] ~ normal(0, 0.5) T[0,]; //value constrained above zero
	for(d in 1:D) for(j in 1:R)  a_r[j,d] ~ normal(0, 0.5) T[0,]; //value constrained above zero
	//difficulty
	to_vector(b_l) ~ normal(0,2);
	to_vector(b_r) ~ normal(0,2);


  //model
	target += reduce_sum( reducer , A , 1 , 
      // data to pass
			Y_l , Y_r , A , S , D , L , Q , R ,
      // parameters to pass
      mA , aK , aK_sigma , bA , delta_j , a_l , b_l , a_r , b_r );
}//model

 generated quantities {
   matrix[N,D] K;
   for ( d in 1:D ) 
      for ( i in 1:N ) 
      K[i,d] = mA[d] +                                           //global intercept - minimum value of knowledge
               aK[i,d] * aK_sigma[d] +                           //individual interecepts -absorbs residual variation   
               bA[S[i], d] * sum (delta_j[ 1 : A[i], d ] );     //effect of age - sex specific

   vector [N * L + N * R ] log_lik;
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
