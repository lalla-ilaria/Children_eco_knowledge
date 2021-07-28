#load required packages
library(rethinking)
library(rlist)

#setwd to "Children_eco_knowledge/"

d <- list.load("2_Data_preparation/processed_data.RData")




###########
#SCHOOLING#
###########
#ordered effect of school years 
#with school as an effect of missed ages of school
school <- ifelse(d$A >= 18, 18, d$A) - 5 - ifelse(d$SY == 0 , 0, d$SY -1) #to caclulate the amunt of school lost
school <- ifelse(school >= 6, 6, school)#to reduce the effect of great loss of school
sch <- list()
#run the model with 1:5 number of dimensions
for (i in 1:5) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
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
  sch[[i]] <- cstan( file = "models/2_schooling.stan", data=dat , chains=1, cores=4 , threads=3, control = list(adapt_delta = 0.99))#, control = list(adapt_delta = 0.99)
  saveRDS(sch[[i]], paste("4_Outputs/posteriors/sch_", i, ".rds", sep = ""))
}


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
  m_ngezi<- cstan( file = "models/1_age.stan", data=dat , chains=1, cores=4 , threads=3  )
  saveRDS(m_ngezi, paste("4_Outputs/posteriors/ngezi.rds", sep = ""))

