#load required packages
library(rethinking)

#loads simulation function
if( !exists( "sim_know", mode = "function")) source("~/../Nextcloud/Project/Children_eco_knowledge/1_simulation_knowledge.R") 
#runs simulation
biglist <- sim_know()
        #Available arguments and defaults
        # N = 30,        #number of individuals
        # M = 100,       #number of questions
        # H=floor(N/5),  #number of households
        # nact = 1,      #number of activites individuals can perform
        # b_A = 0,       #direct effect of age on knowledge
        # b_OS = 0,      #effect of older siblings
        # b_YS = 0,      #effect of younger siblings
        # b_ad = 0,      #effect of adults
        # b_sy = 0,      #effect of years of school (between zero and one) 
        # b_ac = 0,      #effect of activities
        # eta_a = 1.5,   #age dependency of a question (slope of the logistic regression - how fast it improves with x) -exp distributed
        # mean_b = 0,    #difficulty of a question (where the center of the slope is placed over x)
        # sd_b = 1,
        # alpha_c = 0,   #guessing probability (make alpha zero to remove this element, make 5 to have an effect of guessing) - beta distributed
        # beta_c = 5,
        # n_dimensions = 1#either one or more, creates answers that respond differently to activities



#one dimensional model, pooled individuals, pooled questions
dat <- list( N = biglist$N ,              #n individuals
             L = biglist$M ,              #n freelist items
             Q = 50,                      #n questionnaire items
             R = biglist$M/2,             #n image recognition items
             H = max(biglist$HH),         #n households
             A = standardize(biglist$A) , #standardized age
             SY= standardize(biglist$SY), #standardized n of years of school
             OS= standardize(biglist$OS), #standardized n older brothers
             YS= standardize(biglist$YS), #standardized n younger brothers
             AD= standardize(biglist$Nad),#standardized n adults
             HH= biglist$HH,              #integer for household
             am= biglist$activity_matrix, #activities practiced
             C = biglist$nact,            #total n activities
             Y_l = biglist$Y ,                  #answers freelist
             Y_q = biglist$Y[,1:50] ,           #answers questionnaire
             Y_r = biglist$Y[,1:(biglist$M/2)]  #answers picture recognition
             )

#complete model, 1 dimension
m <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/model_code.stan" , data=dat , chains=3, cores=3 )
m_mq <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/model_code_multi_qn.stan" , data=dat , chains=3, cores=3 )
m_mi <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/model_code_multi_ind.stan" , data=dat , chains=3, cores=3 )
compare(m, m_mq, m_mi)

#ordered categorical age
dat <- list( N = biglist$N , 
             L = biglist$M , 
             A = round(biglist$A) - round(min(biglist$A)) + 1 , #standardized age
             Y_l = biglist$Y ,
             O = length(min(round(biglist$A)):max(round(biglist$A))),
             alpha = rep(2,length(min(round(biglist$A)):max(round(biglist$A)))-1)
             )
m_ord <- cstan( file="~/../Nextcloud/Project/Children_eco_knowledge/old_files/model_code_ord_age.stan" , data=dat , chains=3, cores=3 )


#multiple dimensions
biglist <- sim_know(n_dimensions = 3, nact = 9, b_ac = 0, N = 30, M = 100)
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

m_d <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/Children_eco_knowledge/models/model_code_dimensions.stan" , data=dat , chains=3, cores=3, init = 0 )



#WAIC and PSIS
WAIC(m)
PSIS(m)

#sampling
tracerplot(m)
trankplot(m)
par( mfrow = c( 1, 1))

