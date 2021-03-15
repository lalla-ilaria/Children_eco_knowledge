data{
	int D; //n dimensions
	int N; //n individuals
	int L; //n items freelist
	int Q; //n items questionnaire
	int R; //n items image recognition
	int Y_l[N,L]; //answers freelist
  int Y_q[N,Q]; //answers questionnaire
  int Y_r[N,R]; //answers image recognition
	real A[N]; //age of individuals
}

parameters{
  //individual parameters
	matrix[N,D] aK; // individual intercepts on knowledge
	vector<lower=0>[D] bA; // coefficient relating age to knowledge

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
	matrix<lower=0,upper=1>[Q,D] c_q;
}

transformed parameters{
  matrix[N,D] K;
  for ( j in 1:D ) for ( i in 1:N ) K[i,j] =  aK[i,j] + bA[j] * A[i] ; //activity effects
}

model{
  //priors for individual parameters
	to_vector  (aK)  ~ normal(0,1);
	for(i in 1:D) bA[i]  ~ normal(0,0.5) T[0,]; //only positive relations possible

	//priors for item parameters
	to_vector (a_l) ~ lognormal(0, 0.5); //value constrained above zero
	to_vector (a_q) ~ lognormal(0, 0.5); //value constrained above zero
	to_vector (a_r) ~ lognormal(0, 0.5); //value constrained above zero
	to_vector (b_q) ~ normal(0,1);
	to_vector (b_l) ~ normal(0,1);
	to_vector (b_r) ~ normal(0,1);
  to_vector (c_q) ~ beta(5,5);

  //model
	//freelist
	for ( i in 1:N ) {
		for (j in 1:L ) {
			real p = 0;
      for ( d in 1:D ) p = p + a_l[j,d] * (K[i,d] - b_l[j,d]);
      Y_l[i,j] ~ bernoulli( inv_logit( p ));
		}
	}
	//questions
	for ( i in 1:N ) {
		for (j in 1:Q ) {
			real p = 0;
      for ( d in 1:D ) p = p + a_q[j,d] * (K[i,d] - b_q[j,d]);
      Y_q[i,j] ~ bernoulli(c_q[j] + (1 - c_q[j]) * inv_logit( p ));
		}
	}
	//image recognition
	for ( i in 1:N ) {
		for (j in 1:R ) {
			real p = 0;
      for ( d in 1:D ) p = p + a_r[j,d] * (K[i,d] - b_r[j,d]);
      Y_r[i,j] ~ bernoulli( inv_logit( p ));
		}
	}
}
 generated quantities {
   vector [N * L + N * Q + N * R] log_lik;
{
   int k = 1;
    for ( i in 1:N ) {
  		for (j in 1:L ) {
  			real p = 0;
        for ( d in 1:D ) p = p + a_l[j,d] * (K[i,d] - b_l[j,d]);
        log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | inv_logit( p ));
   	  	k = k + 1;
   	  	} // L
      } // N
    for ( i in 1:N ) {
  		for (j in 1:Q ) {
  			real p = 0;
        for ( d in 1:D ) p = p + a_q[j,d] * (K[i,d] - b_q[j,d]);
        log_lik[k] = bernoulli_lpmf( Y_q[ i, j] | c_q[j] + (1 - c_q[j]) * inv_logit( p ));
   	  	k = k + 1;
   	  	} // Q
      } // N
    for ( i in 1:N ) {
  		for (j in 1:R ) {
  			real p = 0;
        for ( d in 1:D ) p = p + a_r[j,d] * (K[i,d] - b_r[j,d]);
        log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | inv_logit( p ));
   	  	k = k + 1;
   	  	} // R
      } // N
  } 
}

