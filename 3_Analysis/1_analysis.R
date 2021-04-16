#load required packages
library(rethinking)
library(rlist)

setwd("../")

d <- list.load("2_Data_preparation/processed_data.RData")


######################
#NUMBER OF DIMENSIONS#
######################
# compare models with different number of dimensions
# data simulated with three dimensions, model comparison should favor model with three dimension
# intercept only for individual knowledge
#simulate data
D <- c(1:3)
m_d <- list()
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
  
  m_d[[i]] <- cstan( file = "models/1_dimensions_intercept_only.stan", data=dat , chains=3, cores=3, init = 0 )
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
             A = round (d$A) , #round age
             Y_l = d$Y_l ,
             O = length (0 : max (round (d$A) ) ),
             alpha = rep( 2, length (1:max( round (d$A) ) -1 ) )
)
m_ord <- cstan( file =  "models/2_model_code_ord_age.stan", data=dat , chains=3, cores=3)
