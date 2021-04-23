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
dat <- list( N = d$N , 
             L = d$L , 
             A = d$A [d$A <= 50] , # age #[d$A <= 50] 
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
             Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
             O = length (0 : 26 ) ,
             alpha = rep( 2, length (0:26 ) -1 ) 
             )

m_ord <- cstan( file =  "models/2_model_code_ord_age.stan", data=dat , chains=3, cores=3)

#continuous age
dat <- list( D = 1,
             N = d$N -1, 
             L = d$L , 
             Q = d$Q ,    #n questionnaire items
             R = d$R ,    #n image recognition items
             A = standardize( d$A [d$A <= 50] ) , #round age
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",] ,
             Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
             Y_r = d$Y_r [rownames(d$Y_r) != "19586",]  #answers picture recognition
              )

m_lin <- stan( file =  "models/model_code_age.stan", data=dat , chains=3, cores=3)
