data{
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
	int HH[N]; //household of individuals
	row_vector[C]am[N] ; //activities performed by individuals
}//data

parameters{
  //individual parameters
	vector[N] aK; // individual intercepts on knowledge
	real <lower=0> bA; // coefficient relating age to knowledge
	real bSY;     //coefficient for school years
	vector[H] aHH;//intercept for household
	real bOS;     //coefficient for older sibs
	real bYS;     //coefficient for younger sibs
	real bAD;     //coefficient for adults in the household
	vector[C] AE; //a vector of coefficients for activities
	
	//item parameters
	//discrimination
	vector<lower=0>[L] a_l;
	vector<lower=0>[Q] a_q;
	vector<lower=0>[R] a_r;
	//difficulty
	vector[L] b_l;
	vector[Q] b_q;
	vector[R] b_r;
	//pseudoguessing
	vector<lower=0,upper=1>[Q] c_q;
	//sigma item parameters
	real<lower=0> a_l_sigma;
	real<lower=0> a_q_sigma;
	real<lower=0> a_r_sigma;
	real<lower=0> b_l_sigma;
	real<lower=0> b_q_sigma;
	real<lower=0> b_r_sigma;
	real<lower=0> c_q_sigma;
}//parameters

transformed parameters{
  vector[N] K;
  for ( i in 1:N ) K[i] =  aK[i]+ bA * A[i] + bSY * SY[i] + // individual effects
                           aHH[HH[i]] + bOS * OS[i] + bYS * YS[i] + bAD * AD[i] + // household and family effects
                           dot_product( AE, am[i]); //activity effects
}//transformed parameters

model{
  //priors for individual parameters
	aK  ~ normal(0,1);
	bA  ~ normal(0,0.5) T[0,]; //only positive relations possible
	bSY ~ normal(0,0.3);
	aHH ~ normal(0,0.5);
	bOS ~ normal(0,0.3);
	bYS ~ normal(0,0.3);
	bAD ~ normal(0,0.3);
	AE  ~ normal(0,0.3);
	
	//priors for item parameters
	a_l ~ lognormal(0, 0.5); //value constrained above zero
	a_q ~ lognormal(0, 0.5); //value constrained above zero
	a_r ~ lognormal(0, 0.5); //value constrained above zero
	b_q ~ normal(0,1);
	b_l ~ normal(0,1);
	b_r ~ normal(0,1);
  c_q ~ beta(5,5);
  //hyperpriors
	a_l_sigma ~ exponential(1);
	b_l_sigma ~ exponential(1);
	a_q_sigma ~ exponential(1);
	b_q_sigma ~ exponential(1);
	c_q_sigma ~ exponential(1);
	a_r_sigma ~ exponential(1);
	b_r_sigma ~ exponential(1);
	
	//model

	//freelist
	for ( i in 1:N ) {
	  vector[L] p = rep_vector(0, L);
	  p =  (a_l * a_l_sigma) .* (K[i] - b_l * b_l_sigma);
		target += bernoulli_logit_lpmf( Y_l[i,] | p );
	}//N

	//questions
	for ( i in 1:N ) {
	  vector[Q] p = rep_vector(0, Q);
    vector[Q] logit_p;
		p = (a_q * a_q_sigma) .* (K[i] - b_q * b_q_sigma);
    // log odds 3PL is log[(Exp[p]+c)/(1-c)]
    logit_p = log( exp(p) + c_q * c_q_sigma ) - log1m( c_q );
    target += bernoulli_logit_lpmf( Y_q[i,] | logit_p );
	}//N
	
	//image recognition
	for ( i in 1:N ) {
	  vector[R] p = rep_vector(0, R);
	  p = (a_r * a_r_sigma) .* (K[i] - b_r * b_r_sigma);
		target += bernoulli_logit_lpmf( Y_r[i,] | p );
	}//N
}//model

generated quantities {
   vector [N * L + N * Q + N * R] log_lik;
{
    int k = 1;
    
    //freelist
		for ( i in 1:N ) {
      vector[L] p = rep_vector(0, L);
	    p = (a_l * a_l_sigma) .* (K[i] - b_l * b_l_sigma);
  		for (j in 1:L ) {
  			log_lik[k] = bernoulli_logit_lpmf( Y_l[ i, j] | p[j] );
   	  	k = k + 1;
   	  	} // L
      } // N
      
    //questions
		for ( i in 1:N ) {
      vector[Q] p = rep_vector(0, Q);
      vector[Q] logit_p;
	    p = (a_q * a_q_sigma) .* (K[i] - b_q * b_q_sigma);
		  logit_p = log( exp(p) + c_q * c_q_sigma ) - log1m( c_q );
  		for (j in 1:Q ) {
  			log_lik[k] = bernoulli_logit_lpmf( Y_q[i,j] | logit_p[j]);
   	  	k = k + 1;
   	  	} // Q
      } // N
    
    //image recognition
		for ( i in 1:N ) {
      vector[R] p = rep_vector(0, R);
	    p = (a_r * a_r_sigma) .* (K[i] - b_r * b_r_sigma);
  		for (j in 1:R ) {
  			log_lik[k] = bernoulli_logit_lpmf( Y_r[ i, j] | p[j] );
   	  	k = k + 1;
   	  	} // R
      } // N

  } 
}

