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
	vector[M] b;
	vector<lower=0>[M] g;
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
	bSY ~ normal(0,0.3);
	aHH ~ normal(0,0.5);
	bOS ~ normal(0,0.3);
	bYS ~ normal(0,0.3);
	bAD ~ normal(0,0.3);
	AE  ~ normal(0,0.3);
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

#evaluation of model
#prec <- precis( m, 2)


#WAIC and PSIS
  WAIC(m)
  PSIS(m)

#sampling
tracerplot(m)
trankplot(m)
par( mfrow = c( 1, 1))


#MULTILEVEL
# model code

model_code_m <- '
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
	real<lower=0> aK_sigma;
	vector[N] aHH; // individual intercepts on knowledge
	real<lower=0> aHH_sigma;
	real <lower=0> bA;      // coefficient relating age to knowledge
	real bSY;     //coefficient for school years
	real bOS;     //coefficient for older sibs
	real bYS;     //coefficient for younger sibs
	real bAD;     //coefficient for adults in the household
	vector[C] AE; //a vector of coefficients for activities
	vector[M] b;
	vector<lower=0>[M] g;
}
transformed parameters{
  vector[N] Ki;
  for ( i in 1:N ) Ki[i] =  aK[i] * aK_sigma + bA * A[i] + bSY * SY[i] + // individual effects
                              aHH[HH[i]] * aHH_sigma + bOS * OS[i] + bYS * YS[i] + bAD * AD[i] + // household and family effects
                              dot_product( AE, am[i]); //activity effects
}
model{
	aK_sigma ~ exponential(1.5);
	aK  ~ normal(0, aK_sigma);
	aHH_sigma ~ exponential(1.5);
	aHH  ~ normal(0, aHH_sigma);
  bA  ~ normal(0,0.5) T[0,]; //only positive relations possible
	bSY ~ normal(0,0.3);
	bOS ~ normal(0,0.3);
	bYS ~ normal(0,0.3);
	bAD ~ normal(0,0.3);
	AE  ~ normal(0,0.3);
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
m_m <- cstan( model_code=model_code_m , data=dat , chains=3, cores=3 )
  
#alternative vectorized way to make loop
for ( i in 1:N ) {
        vector[M] logit_p;
		for (j in 1:M ) {
			logit_p[j] = g_f[j]*(Ki[i]-b_f[j]);
		}
        Y[i,] ~ bernoulli_logit( logit_p );
	}
  