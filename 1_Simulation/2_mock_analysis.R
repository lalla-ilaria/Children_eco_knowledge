#load required packages
library(rethinking)

#loads simulation function
if( !exists( "simulation", mode = "function")) source( "1_Simulation/1_simulation_knowledge.R" )
#runs simulation
        #Available arguments and defaults for simulation 
        # N = 30,        #number of individuals
        # M = 100,       #number of questions
        # H=floor(N/5),  #number of households
        # nact = 1,      #number of activities individuals can perform
        # beta_A = 0,    #direct effect of age on knowledge (note that this can be a coefficient in a linear function, power of exponential or scale in a logistic)
        # beta_OS = 0,   #effect of older siblings
        # beta_YS = 0,   #effect of younger siblings
        # beta_AD = 0,   #effect of adults
        # beta_SY = 0,   #effect of years of school (between zero and one) 
        # beta_AC = 0,   #effect of activities
        # eta_a = 1.5,   #age dependency of a question (slope of the logistic regression - how fast it improves with x) -exp distributed
        # mean_b = 0,    #difficulty of a question (where the center of the slope is placed over x)
        # sd_b = 1,
        # alpha_c = 0,   #guessing probability (make alpha zero to remove this element, make 5 to have an effect of guessing) - beta distributed
        # beta_c = 5,
        # n_dimensions = 1, #either one or more, creates answers that respond differently to activities
        # age_eff = "linear" #defines effect of age on knowledge among linear, exponential, sigmoid

######################
#NUMBER OF DIMENSIONS#
######################
  # compare models with different number of dimensions
  # data simulated with three dimensions, model comparison should favor model with three dimension
  # intercept only for individual knowledge
#simulate data
sim_data <- simulation( n_dimensions = 3, nact = 9, beta_AC = 0.5)
D <- c(1:3)
m_d <- list()
#run the model with 1:3 number of dimensions
for (i in 1:length(D)) {
  dat <- list( D = D[i],
             N = sim_data$N , 
             L = sim_data$M , 
             Q = 50,         #n questionnaire items
             R = sim_data$M/2,#n image recognition items
             A = standardize(sim_data$A) , #standardized age
             Y_l = sim_data$Y ,                  #answers freelist
             Y_q = sim_data$Y[,1:50] ,           #answers questionnaire
             Y_r = sim_data$Y[,1:(sim_data$M/2)]  #answers picture recognition
             )


m_d[[i]] <- cstan( file = "models/1_dimensions_intercept_only.stan", data=dat , chains=3, cores=3, init = 0 )
}
#model comparison
compare(m_d[[1]], m_d[[2]], m_d[[3]], func = "WAIC")
compare(m_d[[1]], m_d[[2]], m_d[[3]], func = "PSIS")
#model comparison with WAIC or PSIS should favor the model with the same number of dimension as the simulated data
plot(compare(m_d[[1]], m_d[[2]], m_d[[3]], func = "WAIC"))

##################
#AGE TOTAL EFFECT#
##################
#ordered categorical age
sim_data <- simulation( N = 100, M = 300, beta_A = 2, age_eff = "sigmoid")
#with each year a step
dat <- list( D = sim_data$n_dimensions,
             N = sim_data$N , 
             L = sim_data$M , 
             Q = 50,         #n questionnaire items
             R = sim_data$M/2,#n image recognition items
             A = round (sim_data$A) , #round age
             S = sim_data$S,
             Y_l = sim_data$Y ,
             Y_q = sim_data$Y[,1:50] ,           #answers questionnaire
             Y_r = sim_data$Y[,1:(sim_data$M/2)],  #answers picture recognition
             O = length (0 : max (round (sim_data$A) ) ),
             alpha = rep( 2, length (1:max( round (sim_data$A) ) ) )
             )
m_ord <- cstan( file =  "models/2_model_code_ord_age.stan", data=dat , chains=3, cores=3)


#linear age and sex
sim_data <- simulation(beta_A = 0.6, beta_AC = 0.5)
sim_data <- simulation( beta_A = 2, age_eff = "sigmoid")
dat <- list( D = sim_data$n_dimensions,
             N = sim_data$N , 
             L = sim_data$M , 
             Q = 50,         #n questionnaire items
             R = sim_data$M/2,#n image recognition items
             A = standardize (sim_data$A) , #round age
             S = ifelse(sim_data$S == 1, 1, 2),
             Y_l = sim_data$Y ,
             Y_q = sim_data$Y[,1:50] ,           #answers questionnaire
             Y_r = sim_data$Y[,1:(sim_data$M/2)]  #answers picture recognition
             )
m_lin <- cstan( file =  "models/1_dimensions_age_all_items.stan", data=dat , chains=3, cores=3)


dat <- list( D = sim_data$n_dimensions,
             N = sim_data$N , 
             L = sim_data$M , 
             Q = 50,         #n questionnaire items
             R = sim_data$M/2,#n image recognition items
             A = (sim_data$A - min(sim_data$A)) / sd(sim_data$A) , #round age
             S = ifelse(sim_data$S == 1, 1, 2),
             Y_l = sim_data$Y ,
             Y_q = sim_data$Y[,1:50] ,           #answers questionnaire
             Y_r = sim_data$Y[,1:(sim_data$M/2)]  #answers picture recognition
             )
m_exp <- cstan( file =  "models/1_decelerating_exp_age.stan", data=dat , chains=3, cores=3)

#sigmoid
dat <- list( D = sim_data$n_dimensions,
             N = sim_data$N , 
             L = sim_data$M , 
             Q = 50,         #n questionnaire items
             R = sim_data$M/2,#n image recognition items
             A = standardize(sim_data$A) , #round age
             S = ifelse(sim_data$S == 1, 1, 2),
             Y_l = sim_data$Y ,
             Y_q = sim_data$Y[,1:50] ,           #answers questionnaire
             Y_r = sim_data$Y[,1:(sim_data$M/2)]  #answers picture recognition
             )
m_sig <- cstan( file =  "models/1_logit_age.stan", data=dat , chains=3, cores=3)

compare(m_lin, m_exp, m_sig)
###################################
#OTHER FACTORS AFFECTING KNOWLEDGE#
###################################
  #structural equation modeling
##############################

  #multiple models and DO calculus
##################################
#continuous age and sex, activities
sim_data <- simulation(beta_A = 0.6, beta_AC = 0.5)
dat <- list( D = sim_data$n_dimensions,
             N = sim_data$N , 
             L = sim_data$M , 
             Q = 50,         #n questionnaire items
             R = sim_data$M/2,#n image recognition items
             C = sim_data$nact,
             A = standardize (sim_data$A) , #round age
             S = ifelse(sim_data$S == 1, 1, 2),
             AM = sim_data$activity_matrix,
             Y_l = sim_data$Y ,
             Y_q = sim_data$Y[,1:50] ,           #answers questionnaire
             Y_r = sim_data$Y[,1:(sim_data$M/2)]  #answers picture recognition
             )
m_act <- cstan( file =  "models/2_activities.stan", data=dat , chains=3, cores=3)

#schooling
sim_data <- simulation(beta_A = 0.6, beta_AC = 0.5)
dat <- list( D = sim_data$n_dimensions,
             N = sim_data$N , 
             L = sim_data$M , 
             Q = 50,         #n questionnaire items
             R = sim_data$M/2,#n image recognition items
             A = standardize (sim_data$A) , #round age
             S = ifelse(sim_data$S == 1, 1, 2),
             SY = standardize(sim_data$SY),
             Y_l = sim_data$Y ,
             Y_q = sim_data$Y[,1:50] ,           #answers questionnaire
             Y_r = sim_data$Y[,1:(sim_data$M/2)]  #answers picture recognition
             )
m_sch <- cstan( file =  "models/2_schooling.stan", data=dat , chains=3, cores=3)


