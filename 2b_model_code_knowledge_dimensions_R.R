library(rethinking)

#loads simulation function
if(!exists("sim_know", mode = "function")) source("~/Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 
#runs simulation
biglist_d <- sim_know( n_dimensions = 2, nact = 2, b_ac = c( 0.2, 0.9), N = 100, M = 100)


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
	bA ~ normal( 0 , 0.5 );
  to_vector( b ) ~ normal(0,1);
  to_vector( g ) ~ normal(0,1);

	for ( i in 1:N ) {
		for ( j in 1:M ) {
			real p = 0;
            for ( k in 1:D ) p = p + g[j,k] * ( K[i,k] - b[j,k] );
			Y[i,j] ~ bernoulli_logit( p );
		}
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


# 
# Warning: 574 of 1500 (38.0%) transitions ended with a divergence.
# This may indicate insufficient exploration of the posterior distribution.
# Possible remedies include: 
#   * Increasing adapt_delta closer to 1 (default is 0.8) 
#   * Reparameterizing the model (e.g. using a non-centered parameterization)
#   * Using informative or weakly informative prior distributions 
# 
# 926 of 1500 (62.0%) transitions hit the maximum treedepth limit of 11 or 2^11-1 leapfrog steps.
# Trajectories that are prematurely terminated due to this limit will result in slow exploration.
# Increasing the max_treedepth limit can avoid this at the expense of more computation.
# If increasing max_treedepth does not remove warnings, try to reparameterize the model.
