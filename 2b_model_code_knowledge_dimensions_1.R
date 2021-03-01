library(rethinking)

#loads simulation function
if(!exists("sim_know", mode = "function")) source("~/Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 
#runs simulation
biglist_d <- sim_know( n_dimensions = 3, nact = 9, b_ac = c( 0.2, 0.5, 0.9))


# model code

model_code_d <- '
data{
	int N;
	int M;
	int Y[N,M];
	real A[N];
}
parameters{
	vector[N] K0; // individual intercepts on knowledge
	real bA; // coefficient relating age to knowledge
	vector[M] b_1;
	vector<lower=0>[M] g_1;
  vector[M] b_2;
	vector<lower=0>[M] g_2;
  vector[M] b_3;
	vector<lower=0>[M] g_3;
}
transformed parameters{
  vector[N] K1;
  vector[N] K2;
  vector[N] K3;
  for ( i in 1:N ) K1[i] =  K0[i] + bA*A[i] ;// 
  for ( i in 1:N ) K2[i] =  K0[i] + bA*A[i] ;// 
  for ( i in 1:N ) K3[i] =  K0[i] + bA*A[i] ;// 
  }
model{
	K0 ~ normal(0,1);
	bA ~ normal(0,0.5);
  b_1 ~ normal(0,1);
	g_1 ~ normal(0,1);
	b_2 ~ normal(0,1);
  g_2 ~ normal(0,1);
	b_3 ~ normal(0,1);
	g_3 ~ normal(0,1);
	
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit( g_1[j] *(K1[i]-b_1[j]) + g_2[j] *(K2[i]-b_2[j]) + g_3[j] *(K3[i]-b_3[j]) );
			Y[i,j] ~ bernoulli( p );
		}
	}
}
'

###use to compile the model without the data, then you can pass new data to it
stan_model( model_code = model_code_d )


#remember to make sure to get the data out of the right biglist
dat <- list( N = biglist_d$N , 
             M = biglist_d$M , 
             Y = biglist_d$Y , 
             A = standardize(biglist_d$A) , 
             SY= standardize(biglist_d$SY),
             am = biglist_d$activity_matrix,
             C = biglist_d$nact
             )

#save different model results with different names!
m_d <- cstan( model_code=model_code_d , data=dat , chains=1 ) #use cstan cause it uses commandstan


post_d <- extract.samples(m_d)


traceplot(m_d)
precis(m_d)
