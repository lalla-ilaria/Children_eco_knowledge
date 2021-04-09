data{
	int N; //n individuals
	int L; //n items freelist
	int Y_l[N,L]; //answers freelist
	real A[N]; //age of individuals
	real SY[N];//school years of individuals
}//data

parameters{
  //individual parameters
	vector[N] aK; // individual intercepts on knowledge
	real <lower=0> bA; // coefficient relating age to knowledge
	real bSY;     //coefficient for school years
	
	// for SY
	real a_sy;
	real b_sy;
	real sigma_sy;

	//item parameters
	//discrimination
	vector<lower=0>[L] a_l;
	//difficulty
	vector[L] b_l;
}//parameters

transformed parameters{
  vector[N] K;
  for ( i in 1:N ) K[i] =  aK[i] + bA * A[i] + bSY * SY[i] ; 
}//transformed parameters

model{
    vector[N] mu_sy;
  //priors for individual parameters
	aK  ~ normal(0,1);
	bA  ~ normal(0,0.5) T[0,]; //only positive relations possible
	bSY ~ normal(0,0.3);

  //priors for sy submodel
  a_sy ~ normal(0,1);
  b_sy ~ normal(0,1);
  sigma_sy ~ exponential(1);
    
	//priors for item parameters
	a_l ~ lognormal(0, 0.5); //value constrained above zero
	b_l ~ normal(0,1);

  //model
  //SY
  for (i in 1:N){
    mu_sy[i] = a_sy + b_sy * A[i];
    SY[i] ~ normal(mu_sy[i] , sigma_sy);
  }
 
	//freelist
	for ( i in 1:N ) {
	  vector[L] p = rep_vector(0, L);
	  p = a_l .* (K[i] - b_l);
		target += bernoulli_logit_lpmf( Y_l[i,] | p );
	}//N
}//model

 generated quantities {
   vector [N * L ] log_lik;
{		
   int k = 1;
   
    //freelist
		for ( i in 1:N ) {
      vector[L] p = rep_vector(0, L);
	    p = a_l .* (K[i] - b_l);
  		for (j in 1:L ) {
  			log_lik[k] = bernoulli_logit_lpmf( Y_l[ i, j] | p[j] );
   	  	k = k + 1;
   	  	} // L
      } // N
  }
}//generated quantities

