library(rethinking)

#loads simulation function
if( !exists( "sim_know", mode = "function")) source("~/Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 
#runs simulation
biglist <- sim_know()

#one dimensional model, pooled individuals, pooled questions
dat <- list( N = biglist$N , #n individuals
             L = biglist$M , #n freelist items
             Q = 50,         #n questionnaire items
             R = biglist$M/2,#n image recognition items
             H = max(biglist$HH), #n households
             A = standardize(biglist$A) , #standardized age
             SY= standardize(biglist$SY), #standardized n of years of school
             OS= standardize(biglist$OS), #standardized n older brothers
             YS= standardize(biglist$YS), #standardized n younger brothers
             AD= standardize(biglist$Nad),#standardized n adults
             HH= biglist$HH, #integer for household
             am= biglist$activity_matrix, #activities practiced
             C = biglist$nact, #total n activities
             Y_l = biglist$Y ,                  #answers freelist
             Y_q = biglist$Y[,1:50] ,           #answers questionnaire
             Y_r = biglist$Y[,1:(biglist$M/2)]  #answers picture recognition
             )

m <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/model_code.stan" , data=dat , chains=3, cores=3 )
m_mq <- cstan( file="model_code_multi_qn.stan" , data=dat , chains=3, cores=3 )
m_mi <- cstan( file="model_code_multi_ind.stan" , data=dat , chains=3, cores=3 )
compare(m, m_mq, m_mi)

#ordered categorical age
dat <- list( N = biglist$N , 
             L = biglist$M , 
             A = round(biglist$A) , #standardized age
             Y_l = biglist$Y ,
             O = length(min(round(biglist$A)):max(round(biglist$A))),
             alpha = rep(2,length(min(round(biglist$A)):max(round(biglist$A)))-1)
             )
m_ord <- cstan( file="model_code_ord_age.stan" , data=dat , chains=3, cores=3 )



#multiple dimensions
biglist <- sim_know(n_dimensions = 3, nact = 3, b_ac = c( 0.5), N = 30, M = 100)
dat <- list( D = biglist$n_dimensions,
             N = biglist$N , 
             L = biglist$M , 
             Q = biglist$M ,
             R = biglist$M ,
             H = max(biglist$HH),
             A = standardize(biglist$A) , #standardized age
             SY= standardize(biglist$SY), #standardized n of years of school
             OS= standardize(biglist$OS),
             YS= standardize(biglist$YS),
             AD= standardize(biglist$Nad),
             HH= biglist$HH, #integer for household
             am= biglist$activity_matrix,
             C = biglist$nact,
             Y_l = biglist$Y , 
             Y_q = biglist$Y , 
             Y_r = biglist$Y
             )

m_d <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/model_code_dimensions_small.stan" , data=dat , chains=3, cores=3 )

# Chain 3   Stan can't start sampling from this initial value.
# Chain 3 Initialization between (-2, 2) failed after 100 attempts. 
# Chain 3  Try specifying initial values, reducing ranges of constrained values, or reparameterizing the model.
# Chain 3 Initialization failed.
# Warning: Chain 3 finished unexpectedly!

#WAIC and PSIS
WAIC(m)
PSIS(m)

#sampling
tracerplot(m)
trankplot(m)
par( mfrow = c( 1, 1))

