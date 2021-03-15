library(rethinking)

#loads simulation function
if(!exists("sim_know", mode = "function")) source("~/Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 
#runs simulation
biglist_d <- sim_know( n_dimensions = 3, nact = 3, b_ac = c( 0.5), N = 30, M = 100)


# model code

model_code_d <- '
data{
	int N;
	int M;
	int D; //n dimensions
	int Y[N,M];
	real A[N];
}
parameters{
	matrix[N,D] aK; // individual intercepts on knowledge
	vector<lower=0>[D] bA; // coefficient relating age to knowledge
	matrix[M,D] b;
	matrix<lower=0>[M,D] g;
}
transformed parameters{
    matrix[N,D] K;
    for ( j in 1:D ) for ( i in 1:N ) K[i,j] = aK[i,j] + bA[j]*A[i] ;// 
}
model{
	to_vector( aK ) ~ normal(0,1);
	for(i in 1:D) bA[i] ~ normal( 0 , 0.5 ) T[0,];
  to_vector( b ) ~ normal(0,1);
  to_vector( g ) ~ lognormal(0, 0.5);

	for ( i in 1:N ) {
		for ( j in 1:M ) {
			real p = 0;
            for ( k in 1:D ) p = p + g[j,k] * ( K[i,k] - b[j,k] );
			Y[i,j] ~ bernoulli_logit( p );
		}
	}
}
generated quantities {
   vector [N * M ] log_lik;
 {
   int t = 1;
    for ( i in 1:N ) {
  		for (j in 1:M ) {
  			real p = 0;
            for ( k in 1:D ) {
            p =  p + g[j,k] * ( K[i,k] - b[j,k] ); }
			log_lik[t] = bernoulli_lpmf( Y[ i, j] | inv_logit(p) );
   	  	t = t + 1;
         // D
   	  	} // M
       } // N
  } 
}
'

#remember to make sure to get the data out of the right biglist
dat <- list( N = biglist_d$N , 
             M = biglist_d$M , 
             Y = biglist_d$Y , 
             A = standardize(biglist_d$A) , 
             SY= standardize(biglist_d$SY),
             am = biglist_d$activity_matrix,
             C = biglist_d$nact,
             D = biglist_d$n_dimensions
             )

#save different model results with different names!
m_d <- cstan( model_code = model_code_d , data = dat , chains = 3, cores = 3) # ,control = list(adapt_delta = 0.9, max_treedepth = 15) use cstan cause it uses commandstan


post_d <- extract.samples(m_d)


tracerplot(m_d,pars="K")

trankplot(m_d)
precis(m_d, depth = 2)

