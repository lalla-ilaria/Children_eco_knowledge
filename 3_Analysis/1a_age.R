#load required packages
library(rethinking)
library(rlist)

#setwd to "Children_eco_knowledge/"

d <- list.load("2_Data_preparation/processed_data.RData")


m_age <- list()
#run the model with 1:5 number of dimensions
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
  m_age[[i]] <- stan( file = "models/1_age.stan", data=dat , chains=1, cores=1 )
}

waics_age <- compare(m_age[[1]], m_age[[2]], m_age[[3]], m_age[[4]], m_age[[5]])

#save
saveRDS(m_age[[1]], "4_Outputs/posteriors/age_1.rds").
saveRDS(m_age[[2]], "4_Outputs/posteriors/age_2.rds").
saveRDS(m_age[[3]], "4_Outputs/posteriors/age_3.rds").
saveRDS(m_age[[4]], "4_Outputs/posteriors/age_4.rds").
saveRDS(m_age[[5]], "4_Outputs/posteriors/age_5.rds").
post_1 <- extract.samples(m_age[[1]])
post_2 <- extract.samples(m_age[[2]])
post_3 <- extract.samples(m_age[[3]])
post_4 <- extract.samples(m_age[[4]])
post_5 <- extract.samples(m_age[[5]])
save( waics_f, file = "4_Outputs/posteriors/waics_age.Rda")
