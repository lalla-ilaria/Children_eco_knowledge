#load required packages
library(rethinking)
library(rlist)

#setwd to "Children_eco_knowledge/"

d <- list.load("2_Data_preparation/processed_data.RData")

#####################
#FOR RICHARD TO TEST#
#####################
dat <- list( D = 3,    #loop through dimensions
             N = as.integer(d$N - 1) , 
             L = d$L , 
             A = d$A [d$A <= 50] , # age #[d$A <= 50] 
             S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
             Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
             O = length (0 : 26 ) ,
             alpha = rep( 0.5, length (0:26 ) -1 ) 
)
m_3 <- stan( file = "models/3_dim_analysis_freelist_only_multiind.stan", data=dat , chains=1, cores=1, init = 0 )


##########################
#ANALYSIS WITH DIMENSIONS#
##########################


##########
#FREELISTS
##########

#####
#age and sex

m_f <- list()
#run the model with 1:5 number of dimensions
for (i in 1:5) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  m_f[[i]] <- stan( file = "models/3_dim_analysis_freelist_only_multiind.stan", data=dat , chains=1, cores=1, init = 0 )
}

#model comparison
waics_f <- compare(m_f[[1]], m_f[[2]], m_f[[3]], m_f[[4]], m_f[[5]])

#save
post_1 <- extract.samples(m_d[[1]])
post_2 <- extract.samples(m_d[[2]])
post_3 <- extract.samples(m_d[[3]])
post_4 <- extract.samples(m_d[[4]])
post_5 <- extract.samples(m_d[[5]])
save( waics_f, file = "4_Outputs/posteriors/waics.Rda")

#####
#age, sex and activities

m_fa <- list()
post_fa <- list()

#run the model with 1:5 number of dimensions
for (i in 1:5) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
               C = ncol(d$amh), 
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               AM = d$amh [rownames(d$amh) != "19586",],
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  m_fa[[i]] <- stan( file = "models/3_dim_analysis_activities_freelist_only.stan", data=dat , chains=1, cores=1, init = 0 )
  post_fa[[i]] <- extract.samples(m_fa[[i]])
  save(post_fa[[i]] , file = paste("4_Outputs/posteriors/post_fa", i, ".Rda", sep = ""))
  
}
waics_fa <- compare(m_fa[[1]], m_fa[[2]], m_fa[[3]], m_fa[[4]], m_fa[[5]])

#save 
post_a1 <- extract.samples(m_da[[1]])
post_a2 <- extract.samples(m_da[[2]])
post_a3 <- extract.samples(m_da[[3]])
post_a4 <- extract.samples(m_da[[4]])
post_a5 <- extract.samples(m_da[[5]])
save( waics_fa, file = "4_Outputs/posteriors/waics_fa.Rda")


##################
#IMAGE RECOGNITION
##################
m_r <- list()
post_r <- list()
#run the model with 1:3 number of dimensions
for (i in 1:5) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$R , 
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               Y_l = d$Y_r [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  m_r[[i]] <- stan( file = "models/3_dim_analysis_freelist_only_multiind.stan", data=dat , chains=1, cores=1, init = 0 )
  post_r[[i]] <- extract.samples(m_r[[i]])
  save(post_r[[i]] , file = paste("4_Outputs/posteriors/post_r", i, ".Rda", sep = ""))
  
}
waics_r <- compare(m_r[[1]], m_f[[2]], m_r[[3]], m_r[[4]], m_r[[5]])
save( waics_f, file = "4_Outputs/posteriors/waics.Rda")


m_ra <- list()
post_ra <- list()
#run the model with 1:3 number of dimensions
for (i in 1:5) {
  dat <- list( D = i,    #loop through dimensions
               N = as.integer(d$N - 1) , 
               L = d$L , 
               C = ncol(d$amh), 
               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
               AM = d$amh [rownames(d$amh) != "19586",],
               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
               O = length (0 : 26 ) ,
               alpha = rep( 0.5, length (0:26 ) -1 ) 
  )
  m_fa[[i]] <- stan( file = "models/3_dim_analysis_activities_freelist_only.stan", data=dat , chains=1, cores=1, init = 0 )
  post_fa[[i]] <- extract.samples(m_fa[[i]])
  save(post_fa[[i]] , file = paste("4_Outputs/posteriors/post_fa", i, ".Rda", sep = ""))
  
}
waics_fa <- compare(m_fa[[1]], m_fa[[2]], m_fa[[3]], m_fa[[4]], m_fa[[5]])
save( waics_fa, file = "4_Outputs/posteriors/waics_fa.Rda")

