library(rethinking)

#compiles the simulation
if(!exists("sim_know", mode = "function")) source("~/Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 

# model code

model_code <- '
data{
	int N;
	int M;
	int Y[N,M];
	real A[N];
}
parameters{
	vector[N] K0; // individual intercepts on knowledge
	real bA; // coefficient relating age to knowledge
	vector[M] b;
	vector<lower=0>[M] g;
}
transformed parameters{
  vector[N] Ki;
  for ( i in 1:N ) Ki[i] = inv_logit( K0[i] + bA*A[i] ); //removed inv_logit 
}
model{
	K0 ~ normal(0,1);
	bA ~ normal(0,0.5);
	g ~ normal(0,1);
	b ~ normal(0,1);
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit(g[j]*(Ki[i]-b[j]));
			Y[i,j] ~ bernoulli( p );
		}
	}
}
'

###use to compile the model without the data, then you can pass new data to it
stan_model( model_code = model_code )


#remember to make sure to get the data out of the right biglist
dat <- list( N=biglist$N, M=biglist$M , Y=biglist$Y , A=standardize(biglist$A) )

#save different model results with different names!
m <- stan( model_code=model_code , data=dat , chains=1 )

prec <- precis(m,2)

post <- extract.samples(m)


