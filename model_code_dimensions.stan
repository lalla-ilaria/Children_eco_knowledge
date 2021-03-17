data{
	int D; //n dimensions
	int N; //n individuals
	int L; //n items freelist
	int Q; //n items questionnaire
	int R; //n items image recognition
	int C; //n activities
	int H; //n households
	int Y_l[N,L]; //answers freelist
  int Y_q[N,Q]; //answers questionnaire
  int Y_r[N,R]; //answers image recognition
	real A[N]; //age of individuals
	real SY[N];//school years of individuals
	real OS[N];//n of older sibs of individuals
	real YS[N];//n of younger sibs of individuals
	real AD[N];//n of adults in the household of individuals
	int  HH[N]; //household of individuals
	row_vector[C] am[N] ; //activities performed by individuals
}//data

parameters{
  //individual parameters
	matrix[N,D] aK; // individual intercepts on knowledge
	vector<lower=0>[D] bA; // coefficient relating age to knowledge
	vector[D] bSY;     //coefficient for school years
	matrix[H,D] aHH;//intercept for household
	vector[D] bOS;     //coefficient for older sibs
	vector[D] bYS;     //coefficient for younger sibs
	vector[D] bAD;     //coefficient for adults in the household
	matrix[C,D] AE; //a vector of coefficients for activities
	
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
      K[i,j] = aK[i,j] +  
          bA[j]*A[i] + bSY[j]*SY[i] +
          aHH[HH[i],j] + bOS[j]*OS[i] + bYS[j]*YS[i] + bAD[j]*AD[i] +
          dot_product( AE[,j] , am[i] ); 
}//transformed parameters

model{
  //priors for individual parameters
	to_vector(aK) ~ normal(0,1);
	bA ~ normal(0,0.5); //only positive relations possible
	bSY ~ normal(0,0.3);
	to_vector (aHH) ~ normal(0,0.5);
	bOS ~ normal(0,0.3);
	bYS ~ normal(0,0.3);
	bAD ~ normal(0,0.3);
	to_vector(AE)  ~ normal(0,0.3);
	
	//priors for item parameters
	to_vector(a_l) ~ normal(0, 0.5); //value constrained above zero
	to_vector(a_q) ~ normal(0, 0.5); //value constrained above zero
	to_vector(a_r) ~ normal(0, 0.5); //value constrained above zero
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