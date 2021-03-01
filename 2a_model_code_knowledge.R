library(rethinking)

#loads simulation function
if( !exists( "sim_know", mode = "function")) source("~/Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 
#runs simulation
biglist <- sim_know(nact = 3, b_ac = c(0.5, 1, 2))

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
	bSY ~ normal(0,0.5);
	aHH ~ normal(0,1);
	bOS ~ normal(0,1);
	bYS ~ normal(0,1);
	bAD ~ normal(0,1);
	AE  ~ normal(0,1);
	for (i in 1:M) g[i]  ~ normal(0,1) T[0,]; //value constrained above zero
	b   ~ normal(0,1);
	for ( i in 1:N ) {
		for (j in 1:M ) {
			real p = inv_logit(g[j]*(Ki[i]-b[j]));
			Y[i,j] ~ bernoulli( p );
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
        log_lik[k] = bernoulli_lpmf( Y[ i, j] | p );
   	  	k = k + 1;
   	  	} // M
      } // N
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

#evaluation of model
#prec <- precis( m, 2)


#WAIC and PSIS
  WAIC(m)
  PSIS(m)

#sampling
traceplot(m)
trankplot(m)
par( mfrow = c( 1, 1))


#ERROR MESSAGES - need anyhting?
# Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
# Chain 2 Exception: bernoulli_lpmf: Probability parameter is nan, but must be in the interval [0, 1] (in 'C:/Users/user/AppData/Local/Temp/RtmpGqvYu3/model-222476a636e7.stan', line 32, column 3 to column 27)
# Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
# Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
# 
# Chain 3 Rejecting initial value:
# Chain 3   Log probability evaluates to log(0), i.e. negative infinity.
# Chain 3   Stan can't start sampling from this initial value.

#real p = c[j] + (1-c[j]) *(inv_logit(g[j]*(Ki[i]-b[j]))/ (1+ inv_logit(g[j]*(Ki[i]-b[j]))))
