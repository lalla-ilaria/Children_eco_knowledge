library(rethinking)

#loads simulation function
if( !exists( "sim_know", mode = "function")) source("~/Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 
#runs simulation
biglist <- sim_know()
#remember to make sure to get the data out of the right biglist
dat <- list( N = biglist$N , 
             M = biglist$M , 
             Y = biglist$Y , 
             A = standardize(biglist$A)  #standardized age
             )

#multilevel individual parameters
# model code
model_code <- '
data{
	int N; //n individuals
	int M; //n questions
	int Y[N,M];
	real A[N];
	}
parameters{
  vector[N] aK;               // individual intercepts on knowledge
	real<lower=0> aK_sigma;
	real <lower=0> bA;          // coefficient relating age to knowledge
	vector[M] b;
	vector<lower=0>[M] g;
}
transformed parameters{
  vector[N] Ki;
  for ( i in 1:N ) Ki[i] =  aK[i] * aK_sigma + bA * A[i]; 
}
model{
	aK  ~ normal(0, 1);
	aK_sigma ~ exponential(1);
	bA  ~ normal(0,0.5) T[0,]; //only positive relations possible
	g   ~ lognormal(0, 0.5); //value constrained above zero
	b   ~ normal(0,1);
		for ( i in 1:N ) {
		  for (j in 1:M ) {
			real p = g[j]*(Ki[i]-b[j]);
			Y[i,j] ~ bernoulli_logit( p );
		}
		}
}
 generated quantities {
   vector [N * M] log_lik;
   vector [N * M] prob;
{
   int k = 1;
    for ( i in 1:N ) {
  		for (j in 1:M ) {
  			real p = inv_logit(g[j]*(Ki[i]-b[j]));
  			prob[k] = p;
        log_lik[k] = bernoulli_lpmf( Y[ i, j] | p );
   	  	k = k + 1;
   	  	} // M
      } // N
    } 
    }
'
m_a <- cstan( model_code=model_code , data=dat , chains=3, cores=3 )

#multilevel item parameters  
model_code <- '
data{
	int N; //n individuals
	int M; //n questions
	int Y[N,M];
	real A[N];
	}
parameters{
  vector[N] aK;               // individual intercepts on knowledge
	real <lower=0> bA;          // coefficient relating age to knowledge
	vector[M] b;
	vector<lower=0>[M] g;
	real<lower=0> b_sigma;
	real<lower=0> g_sigma;
}
transformed parameters{
  vector[N] Ki;
  for ( i in 1:N ) Ki[i] =  aK[i] + bA * A[i]; 
}
model{
	aK  ~ normal(0, 1);
	bA  ~ normal(0,0.5) T[0,]; //only positive relations possible
	g   ~ lognormal(0, 0.5); //value constrained above zero
	b   ~ normal(0,1);
	g_sigma ~ exponential(1);
	b_sigma ~ exponential(1);
		for ( i in 1:N ) {
		  for (j in 1:M ) {
			real p = g[j] * g_sigma *(Ki[i]-b[j] * b_sigma);
			Y[i,j] ~ bernoulli_logit( p );
		}
		}
}
 generated quantities {
   vector [N * M] log_lik;
   vector [N * M] prob;
{
   int k = 1;
    for ( i in 1:N ) {
  		for (j in 1:M ) {
  			real p = inv_logit(g[j] * g_sigma *(Ki[i]-b[j] * b_sigma));
  			prob[k] = p;
        log_lik[k] = bernoulli_lpmf( Y[ i, j] | p );
   	  	k = k + 1;
   	  	} // M
      } // N
    } 
    }
'


m_b <- cstan( model_code=model_code , data=dat , chains=3, cores=3 )
#compare
compare(m_a, m_b)
