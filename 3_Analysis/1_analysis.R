#load required packages
library(rethinking)
library(rlist)

#setwd to "Children_eco_knowledge/"

d <- list.load("2_Data_preparation/processed_data.RData")


##########################
#CHANGE WITH AGE, PER SEX#
##########################
age <- list()
#run the model with 1:6 number of dimensions
for (i in 1:6) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  age[[i]] <- cstan( file = "models/1_age.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(age[[i]], paste("4_Outputs/posteriors/age_", i, ".rds", sep = ""))
}

##########################
#ACTIVITIES###############
##########################

act <- list()
#run the model with 1:3 number of dimensions
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               C = ncol(d$amh), 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               A = d$A [d$A <= 50]  ,  #round age [d$A <= 50]
               AM = d$amh [rownames(d$amh) != "19586",],
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  act[[i]] <- cstan( file = "models/2_activities.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99) )
  saveRDS(act[[i]], paste("4_Outputs/posteriors/act_", i, ".rds", sep = ""))
}

##########################
#SCHOOLING################
##########################

#ordered effect of school years 
#with school as an effect of missed ages of school
school <- ifelse(d$A >= 18, 18, d$A) - 5 - ifelse(d$SY == 0 , 0, d$SY -1) #to caclulate the amunt of school lost
school <- ifelse(school >= 6, 6, school)#to reduce the effect of great loss of school
sch <- list()
#run the model with 1:3 number of dimensions
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L ,    #n freelist items 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               SY = 1+school[-60],
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) , 
             Os = length(unique(school)),
             alpha_s = rep(0.5, length(unique(school))-1)
  )
  sch[[i]] <- cstan( file = "models/3_schooling.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(sch[[i]], paste("4_Outputs/posteriors/sch_", i, ".rds", sep = ""))
}

##########################
#FAMILY###################
##########################

#With intercepts for households
d$HH_ord <- as.integer(factor(d$HH, levels = unique(d$HH)))   
hhi <- list()
#run the model with 1:3 number of dimensions
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L ,    #n freelist items
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               H = d$H ,    #n households
               HH = d$HH_ord[d$A <= 50], #household id
               A = d$A  [d$A <= 50], # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]), #sex
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  hhi[[i]] <- cstan( file = "models/4a_family_intercepts.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(hhi[[i]], paste("4_Outputs/posteriors/hhi_", i, ".rds", sep = ""))
}
#Effect of being firstborn
#use subset of data for which data on birth order is present
fst <- list()
#run the model with 1:3 number of dimensions
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = length(d$A [-which(is.na(d$BO))]) , 
               L = d$L , 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = d$A [-which(is.na(d$BO))] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-which(is.na(d$BO))]),
               FS = ifelse(d$BO == 1, 1, 0)[-which(is.na(d$BO))],
               Y_l = d$Y_l [-which(is.na(d$BO)),] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [-which(is.na(d$BO)),] , #answers questionnaire
               Y_r = d$Y_r [-which(is.na(d$BO)),] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  fst[[i]] <- cstan( file = "models/4b_firstborn.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(fst[[i]], paste("4_Outputs/posteriors/fst_", i, ".rds", sep = ""))
}

#Effect of birth order
bor <- list()
#run the model with 1:3 number of dimensions
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = length(d$A [-which(is.na(d$BO))]) , 
               L = d$L , 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = d$A [-which(is.na(d$BO))] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-which(is.na(d$BO))]),
               BO = d$BO [-which(is.na(d$BO))],
               Y_l = d$Y_l [-which(is.na(d$BO)),] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [-which(is.na(d$BO)),] , #answers questionnaire
               Y_r = d$Y_r [-which(is.na(d$BO)),] , #answers picture recognition
               O = length (0 : 26 ) ,
               Ob =  max(d$BO, na.rm = T)  ,
               alpha = rep( 0.5, length (0:26 ) -1 ),
               alpha_b = rep( 0.5, max(d$BO, na.rm = T) -1 )
  )
  bor[[i]] <- cstan( file = "models/4c_birth_order.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(bor[[i]], paste("4_Outputs/posteriors/bor_", i, ".rds", sep = ""))
}

#Effect of 0/1/2 parents
ppc <- list()
#run the model with 1:3 number of dimensions
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               PP = as.integer(1 + d$MP + d$FP) [d$A <= 50],
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
               O = length (0 : 26 ) ,
               Op =  3  ,
               alpha = rep( 0.5, length (0:26 ) -1 ),
               alpha_p = rep( 0.5, 2 )
  )
  ppc[[i]] <- cstan( file = "models/4d_parents_presence.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(ppc[[i]], paste("4_Outputs/posteriors/ppc_", i, ".rds", sep = ""))
}

#Effect of same sex parent
ssp <- list()
#run the model with 1:3 number of dimensions
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               SP = as.integer(ifelse(d$S == "m", 
                                      ifelse(d$FP == 1, 1,0),
                                      ifelse(d$MP == 1, 1, 0)))[d$A <= 50],
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  ssp[[i]] <- cstan( file = "models/4e_samesex_parent.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(ssp[[i]], paste("4_Outputs/posteriors/ssp_", i, ".rds", sep = ""))
}

