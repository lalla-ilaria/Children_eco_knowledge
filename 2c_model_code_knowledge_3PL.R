library(rethinking)

#loads simulation function
if( !exists( "sim_know", mode = "function")) source("~/../Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 
#runs simulation
biglist <- sim_know(alpha_l = 5, beta_l = 15)

# model code

model_code <- '
data{
	int N; //n individuals
	int M; //n questions
	int Y[N,M];
}
parameters{
	vector[N] aK; // individual intercepts on knowledge
	vector[M] b;
	vector<lower=0>[M] g;
	vector<lower=0,upper=1>[M] l;
	
}
transformed parameters{
  vector[N] Ki;
  for ( i in 1:N ) Ki[i] =  aK[i] ; 
}
model{
	aK  ~ normal(0,1);
	for (i in 1:M) g[i]  ~ normal(0,1) T[0,]; //value constrained above zero
	b ~ normal(0,1);
	l ~ beta(5,5);
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit(g[j]*(Ki[i]-b[j]));
			Y[i,j] ~ bernoulli( l[j]+(1-l[j])*p );
		}
	}
}
 generated quantities {
   vector [N * M] log_lik;
{
   int k = 1;
    for ( i in 1:N ) {
  		for (j in 1:M ) {
  			real p = inv_logit(g[j]*(Ki[i]-b[j]));
        log_lik[k] = bernoulli_lpmf( Y[ i, j] | l[j]+(1-l[j])*p );
   	  	k = k + 1;
   	  	} // M
      } // N
    } 
    }
'
  
  ###use to compile the model without the data, then you can pass new data to it
  #stan_model( model_code = model_code )
  
  
  #remember to make sure to get the data out of the right biglist
  dat <- list( N = biglist$N , 
               M = biglist$M , 
               Y = biglist$Y 
  )
  
  #save different model results with different names!
  m <- cstan( model_code=model_code , data=dat , chains=3, cores=3 )
  