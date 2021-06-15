#load required packages
library(rethinking)
library(rlist)

d <- list.load("2_Data_preparation/processed_data.RData")

####################
#1 AGE TOTAL EFFECT#
####################
#ordered categorical age
#with each year a step
dat <- list( D = 1,
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

m_age <- stan( file =  "models/1_age.stan", data=dat , chains=3, cores=3)
post_age <- extract.samples(m_age)
save(post_age, file = "4_Outputs/posteriors/post_age.Rda")

#################
#2 OTHER FACTORS#
#################

#####
#a ACTIVITIES
#####
dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             C = ncol(d$amh), 
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A = d$A [d$A <= 50]  ,  #round age [d$A <= 50]
             AM = d$amh [rownames(d$amh) != "19586",],
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",],  #answers picture recognition
             O = length (0 : 26 ) ,
             alpha = rep( 0.5, length (0:26 ) -1 ) 
)

m_act <- stan( file =  "models/2_activities.stan", data=dat , chains=3, cores=3)
post_act <- extract.samples(m_act)
save(post_act, file = "4_Outputs/posteriors/post_act.Rda")

#####
#b SCHOOLING
#####
#ordered effect of school years 
#with school as an effect of missed ages of school
school <- ifelse(d$A >= 18, 18, d$A) - 5 - ifelse(d$SY == 0 , 0, d$SY -1) #to caclulate the amunt of school lost
school <- ifelse(school >= 6, 6, school)[-60]#to reduce the effect of great loss of school
#with effect of concluding only a certain part of the school career
# school <- ifelse( d$SY <= 1, 0, #no schooling
#           ifelse( d$SY <= 5, 1, #some elementary
#           ifelse( d$SY <= 9, 2, #a lot of elementary
#           ifelse( d$SY <= 11, 3, #some high
#           4 ))))[-60] #a lot of high,

dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A = d$A [d$A <= 50] , # age #[d$A <= 50] 
             SY =  school, #a lot of high,
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",],  #answers picture recognition
             O = length (0 : 26 ) ,
             alpha = rep( 2, length (0:26 ) -1 ), 
             Os = length(unique(school)),
             alpha_s = rep(0.5, length(unique(school))-1)
)

m_sch <- stan( file =  "models/2_schooling.stan", data=dat , chains=3, cores=3)

#####
#c FAMILY
#####
HHs <- data.frame("HH" =sort(unique(d$HH)), "n" = 1:36)
for (i in 1:nrow(HHs)) d$HHs [which(d$HH == HHs$HH[i])] <- HHs$n[i]

#intercept for household
dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             H = d$H ,    #n of households
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A = d$A [d$A <= 50] ,  #round age [d$A <= 50]
             HH = d$HHs [-60], #household of individuals
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",],  #answers picture recognition
             O = length (0 : 26 ) ,
             alpha = rep( 0.5, length (0:26 ) -1 )
)

m_fam <- stan( file =  "models/2_family_intercepts.stan", data=dat , chains=3, cores=3)




########################
#3 NUMBER OF DIMENSIONS#
########################

#####
#freelist only
#####
#if looking only at the freelists items, more than one dimension is favoured
m_d <- list()
#run the model with 1:3 number of dimensions
for (i in 2:5) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
      )
  m_d[[i]] <- stan( file = "models/3_dim_analysis_freelist_only.stan", data=dat , chains=1, cores=1, init = 0 )
  
}
#model comparison
waics <- compare(m_d[[2]], m_d[[3]], m_d[[4]], m_d[[5]])
post_2 <- extract.samples(m_d[[2]])
post_3 <- extract.samples(m_d[[3]])
post_4 <- extract.samples(m_d[[4]])
post_5 <- extract.samples(m_d[[5]])

save( waics, file = "4_Outputs/posteriors/waics.Rda")
save(post_2, file = "4_Outputs/posteriors/post_2.Rda")
save(post_3, file = "4_Outputs/posteriors/post_3.Rda")
save(post_4, file = "4_Outputs/posteriors/post_4.Rda")
save(post_5, file = "4_Outputs/posteriors/post_5.Rda")

save(m_act, file = "4_Outputs/posteriors/m_act.Rda")
save(m_age, file = "4_Outputs/posteriors/m_age.Rda")
save(m_d, file = "4_Outputs/posteriors/m_d.Rda")
