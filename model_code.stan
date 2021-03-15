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
}

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
}

transformed parameters{
  vector[N] K;
  for ( i in 1:N ) K[i] =  aK[i] + bA * A[i] + bSY * SY[i] + // individual effects
                           aHH[HH[i]] + bOS * OS[i] + bYS * YS[i] + bAD * AD[i] + // household and family effects
                           dot_product( AE, am[i]); //activity effects
}

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
  
  //model
	//freelist
	for ( i in 1:N ) {
		for (j in 1:L ) {
			real p = inv_logit(a_l[j] * (K[i] - b_l[j]));
			     if ( p >= 1 ) print(p, K, aK) ;
      Y_l[i,j] ~ bernoulli( p );
		}
	}
	//questions
	for ( i in 1:N ) {
		for (j in 1:Q ) {
			real p = inv_logit(a_q[j] * (K[i] - b_q[j]));
			     if ( c_q[j] + (1 - c_q[j]) * p >= 1 ) print(p, K, aK) ;
			Y_q[i,j] ~ bernoulli(c_q[j] + (1 - c_q[j]) * p);
		}
	}
	//image recognition
	for ( i in 1:N ) {
		for (j in 1:R ) {
			real p = inv_logit(a_r[j] * (K[i] - a_r[j]));
			     if ( p >= 1 ) print(p, K, aK) ;
			Y_r[i,j] ~ bernoulli( p );
		}
	}
}
 generated quantities {
   vector [N * L + N * Q + N * R] log_lik;
{
   int k = 1;
    for ( i in 1:N ) {
  		for (j in 1:L ) {
  			real p = inv_logit(a_l[j] * (K[i] - b_l[j]));
  			log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | p );
   	  	k = k + 1;
   	  	} // L
      } // N
    for ( i in 1:N ) {
  		for (j in 1:Q ) {
  			real p = inv_logit(a_q[j] * (K[i] - b_q[j]));
  			log_lik[k] = bernoulli_lpmf( Y_q[ i, j] | c_q[j] + (1 - c_q[j]) * p );
   	  	k = k + 1;
   	  	} // Q
      } // N
    for ( i in 1:N ) {
  		for (j in 1:R ) {
  			real p = inv_logit(a_r[j] * (K[i] - b_r[j]));
  			log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | p );
   	  	k = k + 1;
   	  	} // R
      } // N
  } 
}

