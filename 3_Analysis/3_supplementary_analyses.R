#load required packages
library(rethinking)
library(rlist)

#setwd to "Children_eco_knowledge/"

d <- list.load("2_Data_preparation/processed_data.RData")

#################
#BY TYPE OF DATA#
#################
#freelist only
age_l <- list()
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  age_l[[i]] <- cstan( file = "models/supplementary_models/l.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(age_l[[i]], paste("4_Outputs/posteriors/age_l_", i, ".rds", sep = ""))
}
#questionnaire only
age_q <- list()
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               Q = d$Q ,    #n questionnaire items
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  age_q[[i]] <- cstan( file = "models/supplementary_models/q.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(age_q[[i]], paste("4_Outputs/posteriors/age_q_", i, ".rds", sep = ""))
}
#image recognition only
age_r <- list()
for (i in 1:3) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               R = d$R ,    #n image recognition items
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  age_r[[i]] <- cstan( file = "models/supplementary_models/r.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(age_r[[i]], paste("4_Outputs/posteriors/age_r_", i, ".rds", sep = ""))
}

######################
#Prior change example#
######################
dat <- list( D = 1,    #loop through dimensions
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
age_bs_1 <- cstan( file = "models/supplementary_models/bs_1.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
age_bs_3 <- cstan( file = "models/supplementary_models/bs_3.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
saveRDS(age_bs_1, "age_bs_1.rds")
saveRDS(age_bs_3, "age_bs_3.rds")

###############
#SAMPLING BIAS#
###############

#Subset of Ngezi  households
dat <- list( D = 1,    #loop through dimensions
               N = length(d$A [d$A <= 50 & d$HH <= 100]) , 
               L = d$L , 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = d$A [d$A <= 50 & d$HH <= 100] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2)[d$A <= 50 & d$HH <= 100]),
               Y_l = d$Y_l [rownames(d$Y_l) != "19586" & d$HH <= 100,] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586" & d$HH <= 100,] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586" & d$HH <= 100,] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
nge<- cstan( file = "models/1_age.stan", data=dat , chains=1, cores=4 , threads=3  )
saveRDS(nge, paste("4_Outputs/posteriors/nge.rds", sep = ""))

#adding distance from research station as a predictor
dat <- list( D = 1,    #loop through dimensions
               N = length(d$A [!is.na(d$HH_dist) & d$A <= 50]) , 
               L = d$L , 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = d$A [!is.na(d$HH_dist) & d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [!is.na(d$HH_dist) & d$A <= 50]),
               HD = standardize(d$HH_dist [ !is.na(d$HH_dist) & d$A <= 50]),
               Y_l = d$Y_l [rownames(d$Y_l) != "19586" & !is.na(d$HH_dist),] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               Y_q = d$Y_q [rownames(d$Y_q) != "19586" & !is.na(d$HH_dist),] , #answers questionnaire
               Y_r = d$Y_r [rownames(d$Y_r) != "19586" & !is.na(d$HH_dist),] , #answers picture recognition
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
dis <- cstan( file = "models/supplementary_models/distance.stan", data=dat , chains=1, cores=4 , threads=3  )
saveRDS(dis, paste("4_Outputs/posteriors/dis.rds", sep = ""))
  
