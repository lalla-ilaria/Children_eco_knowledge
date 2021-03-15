data{
	int N; //n individuals
	int L; //n items freelist
	int O; //n ages
	int Y_l[N,L]; //answers freelist
	int A[N]; //age of individuals
  vector[O-1] alpha; //prior drichlet
}

parameters{
  //individual parameters
	vector[N] aK; // individual intercepts on knowledge
	real <lower=0> bA; // coefficient relating age to knowledge
  simplex[O-1] delta;
	//item parameters
	//discrimination
	vector<lower=0>[L] a_l;
	//difficulty
	vector[L] b_l;
}


transformed parameters{
  vector[N] K;
  vector[O] delta_j;
  delta_j = append_row(0, delta);
  for ( i in 1:N ) K[i] =  aK[i] + bA * sum (delta_j[min(A):A[i] - min(A)+1]) ; //activity effects
}

model{
  //priors for individual parameters
	aK  ~ normal(0,1);
	bA  ~ normal(0,0.5) T[0,]; //only positive relations possible
  delta ~ dirichlet( alpha );
  

	//priors for item parameters
	a_l ~ lognormal(0, 0.5); //value constrained above zero
	b_l ~ normal(0,1);

  //model
	//freelist
	for ( i in 1:N ) {
		for (j in 1:L ) {
			real p = inv_logit(a_l[j] * (K[i] - b_l[j]));
			Y_l[i,j] ~ bernoulli( p );
		}
	}
}
 generated quantities {
   vector [N * L ] log_lik;
{
   int k = 1;
    for ( i in 1:N ) {
  		for (j in 1:L ) {
  			real p = inv_logit(a_l[j] * (K[i] - b_l[j]));
  			log_lik[k] = bernoulli_lpmf( Y_l[ i, j] | p );
   	  	k = k + 1;
   	  	} // L
      } // N
   } 
}

