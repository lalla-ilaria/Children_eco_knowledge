library(rethinking)

#loads simulation function
if( !exists( "sim_know", mode = "function")) source("~/Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 
#runs simulation
biglist <- sim_know()

# model code
model_code <- '
data{
	int N; //n individuals
	int M; //n questions
	int C; //n activities
	int H; //n households
	int Y[N,M];
	real A[N];
	real SY[N];
	real OS[N];//n of older sibs of individuals
	real YS[N];//n of younger sibs of individuals
	real AD[N];//n of adults in the household of individuals
	int HH[N]; //household of individuals
	row_vector[C]am[N] ; //change to number of activities
}
parameters{
	vector[N] aK; // individual intercepts on knowledge
	real <lower=0> bA;      // coefficient relating age to knowledge
	real bSY;     //coefficient for school years
	vector[H] aHH;//intercept for household
	real bOS;     //coefficient for older sibs
	real bYS;     //coefficient for younger sibs
	real bAD;     //coefficient for adults in the household
	vector[C] AE; //a vector of coefficients for activities
	vector[M] b_f;
	vector<lower=0>[M] g_f;
	vector[M] b_q;
	vector<lower=0>[M] g_q;
	vector<lower=0,upper=1>[M] l_q;
	vector[M] b_r;
	vector<lower=0>[M] g_r;
}
transformed parameters{
  vector[N] Ki;
  for ( i in 1:N ) Ki[i] =  aK[i] + bA * A[i] + bSY * SY[i] + // individual effects
                              aHH[HH[i]] + bOS * OS[i] + bYS * YS[i] + bAD * AD[i] + // household and family effects
                              dot_product( AE, am[i]); //activity effects
}
model{
	aK  ~ normal(0,1);
	bA  ~ normal(0,0.5) T[0,]; //only positive relations possible
	bSY ~ normal(0,0.5);
	aHH ~ normal(0,1);
	bOS ~ normal(0,0.5);
	bYS ~ normal(0,1);
	bAD ~ normal(0,1);
	AE  ~ normal(0,1);
	g_f ~ lognormal(0, 0.5); //value constrained above zero
	b_f ~ normal(0,1);
	g_q ~ lognormal(0, 0.5); //value constrained above zero
	b_q ~ normal(0,1);
	l_q ~ beta(5,5);
	g_r ~ lognormal(0, 0.5); //value constrained above zero
	b_r ~ normal(0,1);
	//Freelist
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit(g_f[j]*(Ki[i]-b_f[j]));
			Y[i,j] ~ bernoulli( p );
		}
	}
	//Questions
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit(g_q[j]*(Ki[i]-b_q[j]));
			Y[i,j] ~ bernoulli( l_q[j]+(1-l_q[j])*p );
		}
	}
	//image Recognition
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit(g_r[j]*(Ki[i]-b_r[j]));
			Y[i,j] ~ bernoulli( p );
		}
	}
}
'

###use to compile the model without the data, then you can pass new data to it
stan_model( model_code = model_code )


#remember to make sure to get the data out of the right biglist
dat <- list( N = biglist$N , 
             M = biglist$M , 
             H = max(biglist$HH),
             Y = biglist$Y , 
             A = standardize(biglist$A) , #standardized age
             SY= standardize(biglist$SY), #standardized n of years of school
             OS= standardize(biglist$OS),
             YS= standardize(biglist$YS),
             AD= standardize(biglist$Nad),
             HH= biglist$HH, #integer for household
             am= biglist$activity_matrix,
             C = biglist$nact
             )

  #save different model results with different names!
  m <- cstan( model_code=model_code , data=dat , chains=3, cores=3 )
  