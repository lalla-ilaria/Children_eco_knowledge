#load required packages
library(rethinking)
library(rlist)

d <- list.load("2_Data_preparation/processed_data.RData")


######################
#NUMBER OF DIMENSIONS#
######################
# compare models with different number of dimensions on the data
# all types of data
##########
# only 1 dimension is favored when analyzing all data
D <- c(1:3)
m_da <- list()
#run the model with 1:3 number of dimensions
for (i in 1:length(D)) {
  dat <- list( D = D[i],    #loop through dimensions
               N = d$N ,    #n of individuals 
               L = d$L ,    #n questionnaire items
               Q = d$Q ,    #n questionnaire items
               R = d$R ,    #n image recognition items
               A = standardize(d$A) , #standardized age
               Y_l = d$Y_l , #answers freelist
               Y_q = d$Y_q , #answers questionnaire
               Y_r = d$Y_r   #answers picture recognition
               
  )
  
  m_da[[i]] <- stan( file = "models/1_dimensions_intercept_only_all_items.stan", data=dat , chains=3, cores=3, init = 0 )
}
#model comparison
compare(m_da[[1]], m_da[[2]], m_da[[3]])

#####
#freelist only
##############
#if looking only at the freelists items, more than one dimension is favoured
D <- c(1:3)
m_d <- list()
#run the model with 1:3 number of dimensions
for (i in 1:length(D)) {
  dat <- list( D = D[i],    #loop through dimensions
               N = d$N ,    #n of individuals 
               L = d$L ,    #n questionnaire items
               A = standardize(d$A) , #standardized age
               S = ifelse(d$S == "m", 1, 2),
               Y_l = d$Y_l  #answers freelist
          )
  
  m_d[[i]] <- stan( file = "models/1_dimensions_age_freelist_only.stan", data=dat , chains=3, cores=3, init = 0 )
}
#model comparison
compare(m_d[[1]], m_d[[2]], m_d[[3]])



##################
#AGE TOTAL EFFECT#
##################
#ordered categorical age
#with each year a step
dat <- list( D = 1,
             N = as.integer(d$N - 1) , 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             A = as.integer(d$A [d$A <= 50]) , # age #[d$A <= 50] 
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
             Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
             O = length (0 : 26 ) ,
             alpha = rep( 2, length (0:26 ) -1 ) 
             )

m_ord <- stan( file =  "models/2_model_code_ord_age.stan", data=dat , chains=3, cores=3)
m_ord_relax <- stan( file =  "models/2_model_code_ord_age_relax.stan", data=dat , chains=3, cores=3)

#continuous age
#linear
dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A = standardize( d$A [d$A <= 50] ) ,  #round age [d$A <= 50]
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",]  #answers picture recognition
              )

m_lin <- stan( file =  "models/1_age_sex_all_items.stan", data=dat , chains=3, cores=3)

#with age non standardized
dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A =  d$A [d$A <= 50]  ,  #round age [d$A <= 50]
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",]  #answers picture recognition
)

m_lin_ns <- stan( file =  "models/1_age_sex_all_items.stan", data=dat , chains=3, cores=3)


#decelerating exponential
dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A = (d$A [d$A <= 50] - min(d$A)) / sd(d$A [d$A <= 50]),  #age [d$A <= 50]
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",]  #answers picture recognition
)

m_exp <- stan( file =  "models/1_decelerating_exp_age.stan", data=dat , chains=3, cores=3)
m_exp_min <- stan( file =  "models/1_decelerating_exp_age_minval.stan", data=dat , chains=3, cores=3)
m_exp_relax <-stan(file =  "models/1_decelerating_exp_age_relax.stan", data=dat , chains=3, cores=3)

#sigmoid
dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A = standardize( d$A [d$A <= 50] ) ,  #round age [d$A <= 50]
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",]  #answers picture recognition
)

m_sig <- stan( file =  "models/1_logit_age.stan", data=dat , chains=3, cores=3)
m_sig_min <- stan( file =  "models/1_logit_age_minval.stan", data=dat , chains=3, cores=3)
m_sig_relax <- stan( file =  "models/1_logit_age_relax.stan", data=dat , chains=3, cores=3)

###############
#OTHER FACTORS#
###############

HHs <- data.frame("HH" =sort(unique(d$HH)), "n" = 1:36)
for (i in 1:nrow(HHs)) d$HHs [which(d$HH == HHs$HH[i])] <- HHs$n[i]
#FAMILY
#intercept for household
dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             H = d$H ,    #n of households
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A = standardize( d$A [d$A <= 50] ) ,  #round age [d$A <= 50]
             HH = d$HHs [-60], #household of individuals
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",]  #answers picture recognition
)

m_fam <- stan( file =  "models/2_family.stan", data=dat , chains=3, cores=3)

#ACTIVITIES
dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             C = ncol(d$am), 
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A = standardize( d$A [d$A <= 50] ) ,  #round age [d$A <= 50]
             AM = d$am [rownames(d$Y_l) != "19586",],
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",]  #answers picture recognition
)

m_act <- stan( file =  "models/2_activities.stan", data=dat , chains=3, cores=3)

#SCHOOLING
#linear effect of school years
dat <- list( D = 1,
             N = as.integer(d$N - 1), 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             A = standardize( d$A [d$A <= 50] ) ,  #round age [d$A <= 50]
             SY = standardize(d$SY) [-60] ,
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",], # [rownames(d$Y_l) != "19586",]
             Y_q = d$Y_q [rownames(d$Y_l) != "19586",], #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_l) != "19586",]  #answers picture recognition
)

m_sch <- stan( file =  "models/2_schooling.stan", data=dat , chains=3, cores=3)
