#load required packages
library(rethinking)
library(rlist)

#load data
d <- list.load("2_Data_preparation/processed_data.RData")

#prepare household vector
d$HH_ord <- as.integer(factor(d$HH, levels = unique(d$HH)))   

##########################
#CHANGE WITH AGE, PER SEX#
##########################
age <- list()
#run the model with 1:6 number of dimensions
for (i in 1:5) {
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
  saveRDS(age[[i]], paste("3_Analysis/fit_models/age_", i, ".rds", sep = ""))
}

##########################
#ACTIVITIES###############
##########################

act <- list()
#run the model with 1:3 number of dimensions
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L ,    #n freelist items
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               C = ncol(d$amh), 
               H = d$H ,    #n households
               HH = d$HH_ord[d$A <= 50], #household id
               A = d$A  [d$A <= 50], # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]), #sex
               AM = d$amh [rownames(d$amh) != "19586",],
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  act[[i]] <- cstan( file = "models/2_activities.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99) )
  saveRDS(act[[i]], paste("3_Analysis/fit_models/act_", i, ".rds", sep = ""))
}




##########################
#FAMILY###################
##########################

#each parent present
epp <- list()
#run the model with 1:3 number of dimensions
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L ,    #n freelist items
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               H = d$H ,    #n households
               A = d$A [d$A <= 50], # age #[d$A <= 50] 
               S = as.integer ( ifelse ( d$S == "m", 1, 2) [-60]), #sex
               MP = d$MP [d$A <= 50],  #whether mother is present in same household
               FP = d$FP [d$A <= 50],  #whether father is present in same household              
               HH = d$HH_ord [d$A <= 50], #household id
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  epp[[i]] <- cstan( file = "models/3a_each_parent_presence.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(epp[[i]], paste("3_Analysis/fit_models/epp_", i, ".rds", sep = ""))
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
  fst[[i]] <- cstan( file = "models/3b_firstborn.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(fst[[i]], paste("3_Analysis/fit_models/fst_", i, ".rds", sep = ""))
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
  bor[[i]] <- cstan( file = "models/3c_birth_order.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(bor[[i]], paste("3_Analysis/fit_models/bor_", i, ".rds", sep = ""))
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
               H = d$H ,    #n households
               HH = d$HH_ord[d$A <= 50], #household id
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
  sch[[i]] <- cstan( file = "models/4_schooling.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(sch[[i]], paste("3_Analysis/fit_models/sch_", i, ".rds", sep = ""))
}