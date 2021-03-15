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
	//sigma item parameters
	real<lower=0> a_l_sigma;
	real<lower=0> a_q_sigma;
	real<lower=0> a_r_sigma;
	real<lower=0> b_l_sigma;
	real<lower=0> b_q_sigma;
	real<lower=0> b_r_sigma;
	real<lower=0> c_q_sigma;
}

transformed parameters{
  vector[N] K;
  for ( i in 1:N ) K[i] =  aK[i]+ bA * A[i] + bSY * SY[i] + // individual effects
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
		for (j in 1:L ) {
			real p = inv_logit(a_l[j] * a_l_sigma * (K[i] - b_l[j] * b_l_sigma));
			Y_l[i,j] ~ bernoulli( p );
		}
	}
	//fuestions
	for ( i in 1:N ) {
		for (j in 1:Q ) {
			real p = inv_logit(a_q[j] * a_q_sigma * (K[i] - b_q[j] * b_q_sigma));
			Y_q[i,j] ~ bernoulli(c_q[j] * c_q_sigma + ( 1 - c_q[j] * c_q_sigma) * p);
		}
	}
	//image recognition
	for ( i in 1:N ) {
		for (j in 1:R ) {
			real p = inv_logit(a_r[j] * a_r_sigma * (K[i] - b_r[j] * b_r_sigma));
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
  			real p = inv_logit(a_l[j] * a_l_sigma * (K[i] - b_l[j] * b_l_sigma));
  			log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | p );
   	  	k = k + 1;
   	  	} // L
      } // N
    for ( i in 1:N ) {
  		for (j in 1:Q ) {
  			real p = inv_logit(a_q[j] * a_q_sigma * (K[i] - b_q[j] * b_q_sigma));
  			log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | c_q[j] * c_q_sigma + ( 1 - c_q[j] * c_q_sigma) * p );
   	  	k = k + 1;
   	  	} // Q
      } // N
    for ( i in 1:N ) {
  		for (j in 1:R ) {
  			real p = inv_logit(a_r[j] * a_r_sigma * (K[i] - b_r[j] * b_r_sigma));
  			log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | p );
   	  	k = k + 1;
   	  	} // R
      } // N
  } 
}

