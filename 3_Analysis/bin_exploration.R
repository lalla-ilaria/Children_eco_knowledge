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
#With intercepts for households
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
  saveRDS(hhi[[i]], paste("3_Analysis/fit_models/hhi_", i, ".rds", sep = ""))
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
  saveRDS(ppc[[i]], paste("3_Analysis/fit_models/ppc_", i, ".rds", sep = ""))
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
  saveRDS(ssp[[i]], paste("3_Analysis/fit_models/ssp_", i, ".rds", sep = ""))
}


###########
#FUNCTIONS#
###########

#plot age and sex regressions
plot_age_sex <- function( post , d, dot_col ){
  A_seq <- c( -2 : 3)#vector to loop over
  mus1 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
  for (i in 1:length(A_seq)) {
    mus1[,i] <- apply(post$aK, 1, mean) +  post$bA[,1,] * A_seq[i]                 #calculate regression over the sequence of data A_seq, given the posterior
  } #aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i]
  mus1.mean <- apply(mus1, 2, mean) #calculate average regression line
  mus1.PI <- apply(mus1, 2, PI) #calculate compatibility intervals
  mus2 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
  for (i in 1:length(A_seq)) {
    mus2[,i] <- apply(post$aK, 1, mean) +  post$bA[,2,] * A_seq[i]                 #calculate regression over the sequence of data A_seq, given the posterior
  } #aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i]
  mus2.mean <- apply(mus2, 2, mean)
  mus2.PI <- apply(mus2, 2, PI)
  #plot
  plot( d$A[d$A <= 50], apply(post$K, 2, mean), 
        xlab = "Age", ylab = "Knowledge", xaxt='n', col = dot_col )
  axis(1, c(5, 10, 15, 20, 25), at = )
  A_seq_real <- A_seq  * sd(d$A[d$A <= 50]) + mean(d$A[d$A <= 50])
  lines(A_seq_real, mus1.mean, col = "darkblue")
  shade(mus1.PI, A_seq_real, col = col.alpha("darkblue", 0.2))
  lines(A_seq_real, mus2.mean, col = "darkred")
  shade(mus2.PI, A_seq_real, col =  col.alpha("darkred", 0.2))
  
}


##################################
#plot contrasting counterfactuals#
##################################
#feed the data, the posterior to look at and the set of counterfactuals you want to change (select a vector from the posterior like post_act_1$aAM[,1,1])

plotcontrastingcounterfactuals <- function(d = d, post , dimn = 1, counterfactual_1, counterfactual_2, 
                                           ages = d$A_j[ d$A_j <= 50 ], 
                                           dots = F, s_boys = F, s_girls = F, l_boys = T, l_girls = T ,
                                           boy_col = boycol, girl_col = girlcol, boy_col_2 = "darkgreen", girl_col_2 = "purple") {
  if(length(counterfactual_1)<=1) counterfactual_1 <- rep(counterfactual_1, 150)
  if(length(counterfactual_2)<=1) counterfactual_2 <- rep(counterfactual_2, 150)
  act_names <- c("household chores", "seashells collecting", "birds hunting", "game hunting", "agriculture", 
                 "livestock caring", "fishing", "diving", "algae farming", "cloves picking")
  year_eff <- apply(post$delta_j[,,dimn], 1, cumsum)
  plot(x = ages, 
       y = apply(post$K[,,dimn], 2, mean), 
       xlab = "Age", 
       yaxt='n' ,
       cex.lab=1.8 , 
       cex.axis=1.8 ,
       pch = 19 , 
       cex = ifelse( dots == T, 1.5, 0) , 
       #family = "A",
       col =  alpha( d$sex_col, 0.6 )  )
  axis(side =2, seq (-10, 5, 1), labels = F)
  #with first counterfactual
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,1,dimn]) * apply(year_eff, 1, mean) + mean(counterfactual_1) , 
         type = "l", 
         col = col.alpha( boy_col, alpha = ifelse( s_boys == T | l_boys == T, 0.7, 0) ) )
  shs <- PI(post$mA[,dimn]) + PI(post$bA[,1,dimn]) * apply(year_eff, 1, PI) + PI(counterfactual_1)
  polygon(x = c( 1:nrow(year_eff), nrow(year_eff):1), y =  c(shs[1,], rev(shs[2,])),
          col= alpha(boy_col, ifelse( s_boys == F, 0, 0.3)), border=NA)
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,2,dimn]) * apply(year_eff, 1, mean) +mean(counterfactual_1), 
         type = "l", 
         col = col.alpha( girl_col, alpha = ifelse( s_girls == T | l_girls == T, 0.7, 0)))
  shs <- PI(post$mA[,dimn]) + PI(post$bA[,2,dimn]) * apply(year_eff, 1, PI) + PI(counterfactual_1)
  polygon(x = c( 1:nrow(year_eff), nrow(year_eff):1), y =  c(shs[1,], rev(shs[2,])),
          col= alpha(girl_col, ifelse( s_girls == F, 0, 0.3)), border=NA)
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i,dimn] + post$bA[i,1,dimn] * year_eff[,i] + counterfactual_1[i], 
          type = "l", 
          col = alpha(boy_col, ifelse( l_boys == F, 0, 0.3)))}
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i,dimn] + post$bA[i,2,dimn] * year_eff[,i] + counterfactual_1[i], 
          type = "l", 
          col = alpha(girl_col, ifelse( l_girls == F, 0, 0.3)))}
  
  #with second counterfactual
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,1,dimn]) * apply(year_eff, 1, mean) + mean(counterfactual_2) , 
         type = "l", 
         col = col.alpha( boy_col_2, alpha = ifelse( s_boys == T | l_boys == T, 0.7, 0) ) )
  shs <- PI(post$mA[,dimn]) + PI(post$bA[,1,dimn]) * apply(year_eff, 1, PI) + PI(counterfactual_2)
  polygon(x = c( 1:nrow(year_eff), nrow(year_eff):1), y =  c(shs[1,], rev(shs[2,])),
          col= alpha(boy_col_2, ifelse( s_boys == F, 0, 0.3)), border=NA)
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,2,dimn]) * apply(year_eff, 1, mean) +mean(counterfactual_2), 
         type = "l", 
         col = col.alpha( girl_col_2, alpha = ifelse( s_girls == T | l_girls == T, 0.7, 0)))
  shs <- PI(post$mA[,dimn]) + PI(post$bA[,2,dimn]) * apply(year_eff, 1, PI) + PI(counterfactual_2)
  polygon(x = c( 1:nrow(year_eff), nrow(year_eff):1), y =  c(shs[1,], rev(shs[2,])),
          col= alpha(girl_col_2, ifelse( s_girls == F, 0, 0.3)), border=NA)
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i,dimn] + post$bA[i,1,dimn] * year_eff[,i] + counterfactual_2[i], 
          type = "l", 
          col = alpha(boy_col_2, ifelse( l_boys == F, 0, 0.3)))}
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i,dimn] + post$bA[i,2,dimn] * year_eff[,i] + counterfactual_2[i], 
          type = "l", 
          col = alpha(girl_col_2, ifelse( l_girls == F, 0, 0.3)))}
  
  # title( paste("Practicing", act_names[act] , "in dimension", dimn), adj = 0, cex.main = 1.8)
  legend("bottomright", 
         legend = c("Boys", "Girls"), 
         col = c(boy_col, girl_col), 
         pch = 19, 
         bty = "n", 
         cex = 1.5, 
         text.col = "black", 
         horiz = F , 
         inset = c(0.01, 0.01))
}

######################################
#Plot counterfactual activity effects#
######################################

plotcount_act <- function(d = d, post , dimn, act, dots = T, boy_col = boycol, girl_col = girlcol) {
  act_names <- c("household chores", "seashells collecting", "birds hunting", "game hunting", "agriculture", 
                 "livestock caring", "fishing", "diving", "algae farming", "cloves picking")
  year_eff <- apply(post$delta_j[,,dimn], 1, cumsum)
  plot(x = d$A_j[ d$A_j <= 50 ], 
       y = apply(post$K[,,dimn], 2, mean), 
       xlab = "Age", 
       yaxt='n' ,
       cex.lab=1.8 , 
       cex.axis=1.8 ,
       pch = 19 , 
       cex = ifelse( dots == T, 1.5, 0) , 
       #family = "A",
       col =  alpha( d$sex_col, 0.6 )  )
  axis(side =2, seq (-10, 5, 1), labels = F)
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i,dimn] + post$bA[i,1,dimn] * year_eff[,i] + post$aAM[i,act,dimn], 
          type = "l", 
          col = col.alpha( boy_col, alpha = 0.1))}
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i,dimn] + post$bA[i,2,dimn] * year_eff[,i] + post$aAM[i,act,dimn], 
          type = "l", 
          col = col.alpha( girl_col, alpha = 0.1))}
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,1,dimn]) * apply(year_eff, 1, mean) + mean(post$aAM[,act,dimn]), 
         type = "l", 
         col = col.alpha( boy_col, alpha = 0.7))
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,2,dimn]) * apply(year_eff, 1, mean) + mean(post$aAM[,act,dimn]), 
         type = "l", 
         col = col.alpha( girl_col, alpha = 0.7))
  title( paste("Practicing", act_names[act] , "in dimension", dimn), adj = 0, cex.main = 1.8)
  legend("bottomright", 
         legend = c("Boys", "Girls"), 
         col = c(boy_col, girl_col), 
         pch = 19, 
         bty = "n", 
         cex = 1.5, 
         text.col = "black", 
         horiz = F , 
         inset = c(0.01, 0.01))
}


####################
#LINEAR AGE AND SEX#
####################
post_l <- extract.samples(m_lin) #extract samples

plot_age_sex(post = post_l, d = d, dot_col = d$sex_col)


############
#ORDERED#
#########
Ks <- apply(post$K, 2, mean)
plot(d$A [ d$A <= 50 ], Ks, xlab = "Age", ylab = "Knowledge" )

aKs <- apply(post$aK, 2, mean)
plot(d$A [ d$A <= 50 ], aKs, xlab = "Age", ylab = "Individual effect on knowledge")


#with sex specific age effects
year_eff_1 <- apply(post$delta_j[,,1], 1, cumsum)
year_eff_2 <- apply(post$delta_j[,,2], 1, cumsum)
plot(1:nrow(year_eff), mean(post$mA) + apply(post$bA, 2, mean) * year_eff[,1], type = "l", 
     xlab = "Age", ylab = "Age specific effect on knowledge" )
plot(d$A[ d$A <= 50 ], apply(post$K, 2, mean), xlab = "Age", ylab = "Knowledge", col = d$sex_col)
for (i in 1:50) {
  lines(1:nrow(year_eff_1),  post$mA[i] + post$bA[i,1,] * year_eff_1[,i], type = "l", col = col.alpha( 'darkblue', alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff_2),  post$mA[i] + post$bA[i,2,] * year_eff_2[,i], type = "l", col = col.alpha( 'darkred', alpha = 0.1))
}

#########################
#DIFFERENT AGE FUNCTIONS#
#########################
#exponential
post_e<-extract.samples(m_exp)
post_er<-extract.samples(m_exp_relax)
post_em<-extract.samples(m_exp_min)
post <- post_er
A_seq <- c( -1 : 7 )#vector to loop over
mus1 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
  mus1[,i] <- apply(post$aK, 1, mean) + post$aA[,1,] *( 1- exp( -post$bA[,1,] * A_seq[i]))                 #calculate regression over the sequence of data A_seq, given the posterior
} #aK[i,j] + aA[S[i],j] * ( 1 - exp(-bA[S[i],j]*A[i]));
mus1.mean <- apply(mus1, 2, mean) #calculate average regression line
mus1.PI <- apply(mus1, 2, PI) #calculate compatibility intervals
mus2 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
  mus2[,i] <- apply(post$aK, 1, mean) + post$aA[,2,] *( 1- exp( -post$bA[,2,] * A_seq[i]))                 #calculate regression over the sequence of data A_seq, given the posterior
} #aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i]
mus2.mean <- apply(mus2, 2, mean)
mus2.PI <- apply(mus2, 2, PI)
#plot
plot( d$A[d$A <= 50], apply(post$K, 2, mean), 
      xlab = "Age", ylab = "Knowledge", xaxt='n', col = dot_col )
axis(1, c(5, 10, 15, 20, 25), at = )
A_seq_real <- A_seq * sd(d$A [d$A <= 50]) +  min(d$A) #(d$A [d$A <= 50] - min(d$A)) / sd(d$A [d$A <= 50])* sd(d$A[d$A <= 50]) + mean(d$A[d$A <= 50])
lines(A_seq_real, mus1.mean, col = "darkblue")
shade(mus1.PI, A_seq_real, col = col.alpha("darkblue", 0.2))
lines(A_seq_real, mus2.mean, col = "darkred")
shade(mus2.PI, A_seq_real, col =  col.alpha("darkred", 0.2))

#logistic
post_s<-extract.samples(m_sig)
post_sr<-extract.samples(m_sig_relax)
post_sm<-extract.samples(m_sig_min)
post <- post_s

A_seq <- c( -4 : 4)#vector to loop over
mus1 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
  mus1[,i] <-  apply(post$mA, 1, mean) +  post$cA[,1,] * inv_logit( post$aA[,1,] * ( A_seq[i] - post$bA[,1,]))
} #aK[i,j] + cA[S[i],j] * inv_logit( aA[S[i],j] * ( A[i] - bA[S[i],j] ));
mus1.mean <- apply(mus1, 2, mean) #calculate average regression line
mus1.PI <- apply(mus1, 2, PI) #calculate compatibility intervals
mus2 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
  mus2[,i] <- apply(post$mA, 1, mean) +  post$cA[,2,] * inv_logit( post$aA[,2,] * ( A_seq[i] - post$bA[,2,]))                 #calculate regression over the sequence of data A_seq, given the posterior
} #aK[i,j] + cA[S[i],j] * inv_logit( aA[S[i],j] * ( A[i] - bA[S[i],j] ));
mus2.mean <- apply(mus2, 2, mean)
mus2.PI <- apply(mus2, 2, PI)
#plot
plot( standardize(d$A[d$A <= 50]), apply(post$K, 2, mean), 
      xlab = "Age", ylab = "Knowledge", xaxt='n', col = dot_col )
axis(1, c(5, 10, 15, 20, 25), at = )
A_seq_real <- A_seq # * sd(d$A[d$A <= 50]) + mean(d$A[d$A <= 50])
lines(A_seq_real, mus1.mean, col = "darkblue")
shade(mus1.PI, A_seq_real, col = col.alpha("darkblue", 0.2))
lines(A_seq_real, mus2.mean, col = "darkred")
shade(mus2.PI, A_seq_real, col =  col.alpha("darkred", 0.2))


#####
#Activities
#contrasts sex
plot(NULL, xlim = c(-2, 4), ylim = c(1,4), yaxt='n')
points(mean(post_aget$bA[,1,] - post_aget$bA[,2,]), 1)
points(mean(post_actth$bA[,1,] - post_actth$bA[,2,]), 2)
points(mean(post_age$bA[,1,] - post_age$bA[,2,]), 3)
points(mean(post_acth$bA[,1,] - post_acth$bA[,2,]), 4)
lines(PI(post_aget$bA[,1,] - post_aget$bA[,2,]), c(1, 1))
lines(PI(post_actth$bA[,1,] - post_actth$bA[,2,]), c(2, 2))
lines(PI(post_age$bA[,1,] - post_age$bA[,2,]), c(3,3))
lines(PI(post_acth$bA[,1,] - post_acth$bA[,2,]), c(4,4))
abline(v = 0)
axis(2, c("aget", "actth", "age", "acth" ), at = c(1:4), las = 1)


#SCHOOL
#plot effect of schooling
A_seq <- c( -2 : 3)#vector to loop over
mus1 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
  mus1[,i] <- apply(post_s$aK, 1, mean) + post_s$aS[,1,] + post_s$bA[,1,] * A_seq[i]                #calculate regression over the sequence of data A_seq, given the post_serior
} #aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i]
mus1.mean <- apply(mus1, 2, mean) #calculate average regression line
mus1.PI <- apply(mus1, 2, PI) #calculate compatibility intervals
mus2 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
  mus2[,i] <- apply(post_s$aK, 1, mean) + post_s$aS[,2,] +  post_s$bA[,2,] * A_seq[i]  #need to get the SY from the correlated SY to each A
} #aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i] 
mus2.mean <- apply(mus2, 2, mean)
mus2.PI <- apply(mus2, 2, PI)

mus1.1 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
  mus1.1[,i] <- apply(post_s$aK, 1, mean) + post_s$aS[,1,] + post_s$bA[,1,] * A_seq[i] + post_s$bSY[,1,]*A_seq[i]                #calculate regression over the sequence of data A_seq, given the post_serior
} #aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i] + bSY[S[i],j]*SY[i]
mus1.1.mean <- apply(mus1.1, 2, mean) #calculate average regression line
mus1.1.PI <- apply(mus1.1, 2, PI) #calculate compatibility intervals
mus2.1 <- matrix(nrow = 3000, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
  mus2.1[,i] <- apply(post_s$aK, 1, mean) + post_s$aS[,2,] +  post_s$bA[,2,] * A_seq[i] + post_s$bSY[,2,]*A_seq[i]   #need to get the SY from the correlated SY to each A
} #aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i] + bSY[S[i],j]*SY[i]
mus2.1.mean <- apply(mus2.1, 2, mean)
mus2.1.PI <- apply(mus2.1, 2, PI)

#plot
plot( d$A[d$A <= 50], apply(post_s$K, 2, mean), 
      xlab = "Age", ylab = "Knowledge", xaxt='n', col = dot_col )
axis(1, c(5, 10, 15, 20, 25), at = )
A_seq_real <- A_seq  * sd(d$A[d$A <= 50]) + mean(d$A[d$A <= 50])
lines(A_seq_real, mus1.mean, col = "darkblue")
shade(mus1.PI, A_seq_real, col = col.alpha("darkblue", 0.2))
lines(A_seq_real, mus2.mean, col = "darkred")
shade(mus2.PI, A_seq_real, col =  col.alpha("darkred", 0.2))
lines(A_seq_real, mus1.1.mean, col = "blue")
shade(mus1.1.PI, A_seq_real, col = col.alpha("blue", 0.2))
lines(A_seq_real, mus2.1.mean, col = "red")
shade(mus2.1.PI, A_seq_real, col =  col.alpha("red", 0.2))


############################
#PLOTS FOR DESCRIBING MODEL#
############################
#describe model
#check image recognition
par( mfrow = c( 1, 1))
#knolwedge only 
png(file = "4_Outputs/plots/item_parameter_knowonly.png", width = 600, height = 400)
plot(apply(post$K, 2, mean), rep(0.5, 93),
     pch = 19, 
     col = col.alpha("cornflowerblue", 0.4), 
     xlab = "knowledge in image recognition", 
     ylab = "",
     xlim = c(-11, 4), 
     ylim = c(0, 1))  

dev.off()


a_rs <- apply(post$a_r, 2, mean)
b_rs <- apply(post$b_r, 2, mean)

png(file = "4_Outputs/plots/item_parameter_easy.png", width = 600, height = 400)
n <- 4
curve(inv_logit(a_rs[n] * ( x - b_rs[n])), 
      xlim = c(-11, 4), 
      ylim = c(0, 1), 
      xlab = "knowledge in image recognition", 
      ylab = "p correct answer",
      col = "lightblue", 
      lwd=2)
points(apply(post$K, 2, mean), rep(0.5, 93),
       pch = 19, 
       col = col.alpha("cornflowerblue", 0.4), 
       xlim = c(-11, 4), 
       ylim = c(0, 1))  
dev.off()
png(file = "4_Outputs/plots/item_parameter_hard.png", width = 600, height = 400)
n <- 3
curve(inv_logit(a_rs[n] * ( x - b_rs[n])), 
      xlim = c(-11, 4), 
      ylim = c(0, 1), 
      xlab = "knowledge in image recognition", 
      ylab = "p correct answer",
      col = "lightblue", 
      lwd=2)
points(apply(post$K, 2, mean), rep(0.5, 93),
       pch = 19, 
       col = col.alpha("cornflowerblue", 0.4), 
       xlim = c(-11, 4), 
       ylim = c(0, 1))  
dev.off()


png(file = "4_Outputs/plots/item_parameter_discrimination.png", width = 600, height = 400)
curve(inv_logit(a_rs[1] * ( x - b_rs[1])), 
      xlim = c(-11, 4), 
      ylim = c(0, 1), 
      xlab = "knowledge in image recognition", 
      ylab = "p correct answer",
      col = "lightblue", 
      lwd = 2)
points(apply(post$K, 2, mean), rep(0.5, 93),
       pch = 19, 
       col = col.alpha("cornflowerblue", 0.4), 
       xlim = c(-11, 4), 
       ylim = c(0, 1))  
dev.off()

png(file = "4_Outputs/plots/item_parameter_all.png", width = 600, height = 400)
curve(inv_logit(a_rs[1] * ( x - b_rs[1])), 
      xlim = c(-11, 4), 
      ylim = c(0, 1), 
      xlab = "knowledge in image recognition", 
      ylab = "p correct answer",
      col = col.alpha("lightblue", 0.9))
for(i in 1: length(a_rs)){
  curve(inv_logit(a_rs[i] * ( x - b_rs[i])), 
        col = col.alpha("lightblue", 0.7), 
        add = TRUE)
}
points(apply(post$K, 2, mean), rep(0.5, 93),
       pch = 19, 
       col = col.alpha("cornflowerblue", 0.4), 
       xlim = c(-11, 4), 
       ylim = c(0, 1))  
dev.off()



############
#DIMENSIONS#
############
#age effects by dimensins without dimension specific deltas
year_eff <- apply(post_d$delta_j, 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,1], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i] + post_d$bA[i,1,1] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i] + post_d$bA[i,2,1] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,2], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i] + post_d$bA[i,1,2] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i] + post_d$bA[i,2,2] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,3], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i] + post_d$bA[i,1,3] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i] + post_d$bA[i,2,3] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}


#ANALYSYS
# compare models with different number of dimensions on the data
# all types of data
##########
# only 1 dimension is favored when analyzing all data
# D <- c(1:3)
# m_da <- list()
# #run the model with 1:3 number of dimensions
# for (i in 1:length(D)) {
#  dat <- list( D = D[i],    #loop through dimensions
#               N = as.integer(d$N - 1) , 
#               L = d$L , 
#               Q = d$Q ,    #n questionnaire items
#               R = d$R ,    #n image recognition items
#               A = d$A [d$A <= 50] , # age #[d$A <= 50] 
#               S = as.integer(ifelse(d$S == "m", 1, 2) [-60]),
#               Y_l = d$Y_l [rownames(d$Y_l) != "19586",] , #answers freelist #[rownames(d$Y_l) != "19586",] 
#               Y_q = d$Y_q [rownames(d$Y_q) != "19586",] , #answers questionnaire
#               Y_r = d$Y_r [rownames(d$Y_r) != "19586",] , #answers picture recognition
#               O = length (0 : 26 ) ,
#               alpha = rep( 0.5, length (0:26 ) -1 ) 
#   )
#   
#   m_da[[i]] <- stan( file = "models/1_age.stan", data=dat , chains=3, cores=3, init = 0 )
# }
# #model comparison
# compare(m_da[[1]], m_da[[2]], m_da[[3]])

#### dimensions discriminations
par(mfrow = c(1,1))
plot( c(0,2), c(1,703),col = "white")
llll <- apply(post_age_1$a_l, 2, PI)
for(i in 1: 703) lines(llll[,i], rep(i,2), col = col.alpha("lightblue", 0.7))
abline(v = 0, col = "grey")

par(mfrow = c(1,3))
plot( c(0,2), c(1,703),col = "white")
llll <- apply(post_age_3$a_l[,,1], 2, PI)
for(i in 1: 703) lines(llll[,i], rep(i,2), col = col.alpha("lightblue", 0.7))
abline(v = 0, col = "grey")
plot( c(0,2), c(1,703),col = "white")
llll <- apply(post_age_3$a_l[,,2], 2, PI)
for(i in 1: 703) lines(llll[,i], rep(i,2), col = col.alpha("lightblue", 0.7))
abline(v = 0, col = "grey")
plot( c(0,2), c(1,703),col = "white")
llll <- apply(post_age_3$a_l[,,3], 2, PI)
for(i in 1: 703) lines(llll[,i], rep(i,2), col = col.alpha("lightblue", 0.7))
abline(v = 0, col = "grey")


par(mfrow = c(1,3))
disc <- data.frame( "d1" = apply(post_age_3$a_l[,,1], 2, mean), 
                    "d2" = apply(post_age_3$a_l[,,2], 2, mean), 
                    "d3" = apply(post_age_3$a_l[,,3], 2, mean))
pairs(disc, col = col.alpha("cornflowerblue", 0.5), pch = 19)
plot (  apply(post_age_3$a_l[,,1], 2, mean), apply(post_age_3$a_l[,,2], 2, mean), 
        col = col.alpha("cornflowerblue", 0.7), pch = 19, xlab = "Discrimination 1", ylab = "Discrimination 2")
plot (  apply(post_age_3$a_l[,,1], 2, mean), apply(post_age_3$a_l[,,3], 2, mean), 
        col = col.alpha("cornflowerblue", 0.7), pch = 19, xlab = "Discrimination 1", ylab = "Discrimination 3")
plot (  apply(post_age_3$a_l[,,3], 2, mean), apply(post_age_3$a_l[,,2], 2, mean), 
        col = col.alpha("cornflowerblue", 0.7), pch = 19, xlab = "Discrimination 3", ylab = "Discrimination 2")

png("../individual K by dimension.png", width = 14, height = 6, units = "cm", pointsize = 10,  res = 300)
par(mfrow = c(1,3), mar = c(4, 4, 1, 1))
post <- post_age_3
col_lines <- "lightblue"
col_dots <-  "cornflowerblue"
n_lines <- 150
for (v in 1:3){
  dimn <- v
    a <- apply(post$a_r[,,dimn], 2, mean)
    b <- apply(post$b_r[,,dimn], 2, mean)
  
  curve(inv_logit(a[1] * ( x - b[1])), 
        xlim = c(-11, 4), 
        ylim = c(0, 1), 
        xlab = paste("knowledge dimension", dimn), 
        ylab = "p correct answer", 
        cex.lab=1.5, 
        cex.axis=1.5,
        col = "white")
  for(i in 1: n_lines){
        curve(inv_logit(a[i] * ( x - b[i])), 
                    col = alpha(col_lines, 0.4), 
                    add = TRUE)}
   points(apply(post$K[,,dimn], 2, mean), rep(0.5, 93),
         pch = 19, 
         cex = 0.7, 
         col = alpha(col_dots, 0.2))  

  points(mean(post$K[,7 ,dimn]), 0.5,
         pch = 19, 
         cex = 0.8, 
         col = alpha("darkblue", 0.9))  

}
dev.off()



#dimensions contain similar info
par(mar = c(0,0,0,0), mfrow = c(3,3))
plot(apply(post_age_2$K[,,1], 2, mean), apply(post_age_3$K[,,3], 2, mean), pch = 19, col = "100", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_2$K[,,2], 2, mean), apply(post_age_3$K[,,1], 2, mean), pch = 19, col = "100", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_3$K[,,1], 2, mean), apply(post_age_4$K[,,1], 2, mean), pch = 19, col = "50", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_3$K[,,2], 2, mean), apply(post_age_4$K[,,2], 2, mean), pch = 19, col = "50", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_3$K[,,3], 2, mean), apply(post_age_4$K[,,4], 2, mean), pch = 19, col = "50", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_4$K[,,1], 2, mean), apply(post_age_5$K[,,2], 2, mean), pch = 19, col = "30", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_4$K[,,2], 2, mean), apply(post_age_5$K[,,3], 2, mean), pch = 19, col = "30", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_4$K[,,3], 2, mean), apply(post_age_5$K[,,4], 2, mean), pch = 19, col = "30", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_4$K[,,4], 2, mean), apply(post_age_5$K[,,5], 2, mean), pch = 19, col = "30", axes=FALSE, frame.plot=TRUE)
par(mar = c(5,2,2,2) + 0.1)
