##################
#PRIOR SIMULATION#
##################

#exploration of the prior
curve( inv_logit(rlnorm(1, 0, 0.5) * ( x - rnorm(1, 0, 1) ) ), xlim = c(-4, 4) , ylim = c(0, 1) )
for(i in 1:20)curve( inv_logit(rlnorm(1, 0, 0.5) * ( x - rnorm(1, 0, 1) ) ), add = TRUE)

curve( (rnorm(1, 0, 1) + rnorm(1, 0, 0.5) + abs(rnorm (1, 0, 0.5)) * x + abs(rnorm (1, 0, 0.3)) * (x/3)  ), xlim = c(-5, 5) , ylim = c(-5, 5), ylab = "K" )
for(i in 1:20) curve( (rnorm(1, 0, 1) +rnorm(1, 0, 0.5) + abs(rnorm (1, 0, 0.5)) * x + abs(rnorm (1, 0, 0.3)) * (x/3) ), add = TRUE, ylab = NULL)

#checking priors for declining exponential relation of age
curve( rnorm(1, 1, 1) *(1-exp(-rnorm(1, 1, 1)*x)), xlim = c(0, 5), ylim = c(0, 2) )
for (i in 1:10) curve( rnorm(1, 1, 0.5) *(1-exp(-rnorm(1, 0.5, 0.5)*x)), add = TRUE)

#with logistic
curve( inv_logit(x), xlim = c(-4, 4), ylim = c(0, 2) )
for (i in 1:10) curve( rnorm(1, 1, 0.5) * inv_logit( rnorm(1, 1, 1) * ( x - rnorm(1, 1, 1) )), add = TRUE)


###########
#MODEL FIT#
###########
#add here basic model
precis(m)

#extract posterior
post <- extract.samples(m_sig)

#recover K
plot( sim_data$K, apply( post$K, 2, mean))
abline( 0, 1)
PI <- apply( post$K, 2, PI)
for( i in 1:sim_data$N){
  lines( rep( sim_data$K[i], 2), PI[,i])
}

#recover g
plot( sim_data$a, apply( post$a_l, 2, mean))
abline( 0, 1)
# PI <- apply( post$a_l, 2, PI)
# for( i in 1:sim_data$M){
#   lines( rep( sim_data$a[i], 2), PI[,i])
# }

#recover b
plot( sim_data$b, apply( post$b_l, 2, mean))
abline( 0, 1)
# PI <- apply( post$b, 2, PI)
# for( i in 1:sim_data$M){
#   lines( rep( sim_data$b[i], 2), PI[,i])
# }

#recover l
plot( sim_data$c, apply( post$c, 2, mean))
abline( 0, 1)
PI <- apply( post$l, 2, PI)
for( i in 1:sim_data$M){
  lines( rep( sim_data$l[i], 2), PI[,i])
}


############
#AGE EFFECT#
############
  ############
#LINEAR A
#evaluate A estimation
plot( sim_data$A, apply( post$Ar, 2, mean))
abline( 0, 1)
PI <- apply( post$Ar, 2, PI)
for( i in 1:sim_data$N){
  lines( rep( sim_data$A[i], 2), PI[,i])
}


#bA: evaluate model
  A_seq <- seq (from = -3, to = 3, length.out = 30)
  avg_aK <- apply(post$aK, 1, mean)
  mu <- matrix(nrow = nrow(post$bA), ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
  for (i in 1:length(A_seq)) {
    mu[,i] <- avg_aK + post$bA * A_seq[i]                 #calculate regression over the sequence of data A_seq, given the posterior
  }
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI)
  
  plot( standardize(sim_data$A), sim_data$K, xlab = "st_Age", ylab = "simulated_K")
  lines(A_seq, mu.mean)
  shade(mu.PI, A_seq)

#compare multiple values 
b_A <- c(0.2, 0.5, 0.8)
bA  <- c(NA, NA, NA)
bA_PI <- matrix(nrow = 3, ncol = 2)
 #NB this runs the model multiple times
for (i in 1:length(b_A)) {
  sim_data <- sim_know(b_A = b_A[i])
  dat <- list( N = sim_data$N , 
             M = sim_data$M , 
             H = max(sim_data$HH),
             Y = sim_data$Y , 
             A = standardize(sim_data$A) , #standardized age
             SY= standardize(sim_data$SY), #standardized n of years of school
             OS= standardize(sim_data$OS),
             YS= standardize(sim_data$YS),
             AD= standardize(sim_data$Nad),
             HH= sim_data$HH, #integer for household
             am= sim_data$activity_matrix,
             C = sim_data$nact
             )
  m <- cstan( model_code=model_code , data=dat , chains=3, cores=3 )
  post <- extract.samples(m)
  bA[i] <- apply(post$bA, 1, mean)
  bA_PI[i,] <- PI(post$bA)
}
 #plot to see comparison 
plot(b_A, bA, ylim = c(0, 1))  
for(i in 1:3) {
lines( rep(b_A[i], 2), c (bA_PI[i,]))
}
  

  ######################
#ORDERED CATEGORICAL A

plot (precis(m_ord, 2, pars = "delta_j"))
#it  is very hard to see any difference in the delta values. 
#No matter how the effect of age on knowledge changes with ages, 
#the deltas are not very different. 
#Only by grouping ages in pretty big groups some pattern emerges
post <- extract.samples(m_ord)
#sum deltas per each extracted sample, so that each year/group has the estimated delta per that age/group
year_eff <- apply(post$delta_j, 1, cumsum)
plot(1:nrow(year_eff), year_eff[,1], type = "l")
for (i in 1:ncol(year_eff)) {
 lines(1:nrow(year_eff), year_eff[,i], type = "l", col = col.alpha( 'black', alpha = 0.1))
}

year_eff_tot <- year_eff * mean(post$bA)#multiplying the deltas by the coefficient for age increase, we get a comparable effect
plot(jitter (rep(1:nrow(year_eff), 1500)), year_eff_tot, col = col.alpha("black", 0.3))
#estimated increase per each age/group is very overlapping
mu_year_eff <- apply(year_eff, 1, mean)#make a mean
pi_year_eff <- apply(year_eff, 1, PI)#get distribution center
plot( 1:nrow(year_eff), mu_year_eff)
for (i in 1:nrow(year_eff)) {
  lines(rep(i, 2), pi_year_eff[,i])
}
#min(round(sim_data$A)):max(round(sim_data$A))#need this if you want to plot over actual years
#Only in the most extreme cases the increase appears to be skewed. In most cases the increase is very linear

pairs(m_ord, pars = "delta")

############
#SEX EFFECT#
############
post <- extract.samples(m_lin)
A_seq <- c( -2, 2)
mus1 <- matrix(nrow = 1500, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
mus1[,i] <- apply(post$aK, 1, mean) + post$bA[,1,] * A_seq[i]                 #calculate regression over the sequence of data A_seq, given the posterior
} #aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i]
mus1.mean <- apply(mus1, 2, mean)
mus1.PI <- apply(mus1, 2, PI)
mus2 <- matrix(nrow = 1500, ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
for (i in 1:length(A_seq)) {
mus2[,i] <- apply(post$aK, 1, mean) + post$bA[,2,] * A_seq[i]                 #calculate regression over the sequence of data A_seq, given the posterior
} #aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i]
mus2.mean <- apply(mus2, 2, mean)
mus2.PI <- apply(mus2, 2, PI)

sim_data$sex_col <- ifelse(sim_data$S == 1, "darkblue", "darkred")
plot( standardize (sim_data$A), apply(post$K, 2, mean), 
      xlab = "st age", ylab = "knowledge", col = sim_data$sex_col )
lines(A_seq, mus1.mean, col = "darkblue")
shade(mus1.PI, A_seq, col = col.alpha("darkblue", 0.2))
lines(A_seq, mus2.mean, col = "darkred")
shade(mus2.PI, A_seq, col =  col.alpha("darkred", 0.2))


################
#ADDING POOLING#
################
#compare models with multilevel individuals vs questions
#want to see if the PI around K (but also a and b) is reduced more with pooling over individuals or questions
diffs_K <- list()
diffs_a <- list()
diffs_b <- list()
mean_K <- list()
mean_a <- list()
mean_b <- list()

  sim_data <- sim_know()
#data
  #####
#one dimensional model, pooled individuals, pooled questions
dat <- list( N = sim_data$N , #n individuals
             L = sim_data$M , #n freelist items
             Q = 50,         #n questionnaire items
             R = sim_data$M/2,#n image recognition items
             H = max(sim_data$HH), #n households
             A = standardize(sim_data$A) , #standardized age
             SY= standardize(sim_data$SY), #standardized n of years of school
             OS= standardize(sim_data$OS), #standardized n older brothers
             YS= standardize(sim_data$YS), #standardized n younger brothers
             AD= standardize(sim_data$Nad),#standardized n adults
             HH= sim_data$HH, #integer for household
             am= sim_data$activity_matrix, #activities practiced
             C = sim_data$nact, #total n activities
             Y_l = sim_data$Y ,                  #answers freelist
             Y_q = sim_data$Y[,1:50] ,           #answers questionnaire
             Y_r = sim_data$Y[,1:(sim_data$M/2)]  #answers picture recognition
             )
  #####
  m <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/model_code.stan" , data=dat , chains=3, cores=3 )
  m_mq <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/model_code_multi_qn.stan" , data=dat , chains=3, cores=3 )
  m_mi <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/model_code_multi_ind.stan" , data=dat , chains=3, cores=3 )
  compare(m, m_mq, m_mi) #pooling over questions seems to improve out of sample estimation, but this is not really relevant here

post <- list()
 post[[1]] <- extract.samples(m)
 post[[2]] <- extract.samples(m_mq)
 post[[3]] <- extract.samples(m_mi)
 

PIs <- list()
  for(i in 1:3) {
   PIs[[i]] <- apply( post[[i]]$K, 2, PI)
   diffs_K[[i]] <- (mean(diff(PIs[[i]])))
   mean_K[[i]] <- apply( post[[i]]$K, 2, mean)
  } #estimated Ks are very similar across the three models, 
    #but pooling over questions increases considerably uncertainty of the measure. 
    #Pooling over individuals seems to reduce it a bit
 plot(sim_data$K, mean_K[[1]], col = "red")
 points(sim_data$K, mean_K[[2]], col = "blue")
 points(sim_data$K, mean_K[[3]], col = "green")
#same for b and a
 PIs <- list()
 for(i in 1:3) {
   PIs[[i]] <- apply( post[[i]]$a_l, 2, PI)
   diffs_a[[i]] <- (mean(diff(PIs[[i]])))
   mean_a[[i]] <- apply( post[[i]]$a_l, 2, mean)
 }
 plot(sim_data$a, mean_a[[1]], col = "red")
 points(sim_data$a, mean_a[[2]], col = "blue")
 points(sim_data$a, mean_a[[3]], col = "green")
 PIs <- list()
 for(i in 1:3) {
   PIs[[i]] <- apply( post[[i]]$b_l, 2, PI)
   diffs_b[[i]] <- (mean(diff(PIs[[i]])))
   mean_b[[i]] <- apply( post[[i]]$b_l, 2, mean)
  }


###############################
#TESTING MODEL WITH DIMENSIONS#
###############################
#multiple dimensions  
post_d <- extract.samples(m_d[[2]])

par( mfrow = c( sim_data_d$n_dimensions, sim_data_d$n_dimensions))

for (j in 1:sim_data_d$n_dimensions) {
  for(k in 1: sim_data_d$n_dimensions) {
  plot( sim_data_d$K[,j], apply( post_d$K[,,k], 2, mean), xlab = paste("simulated K n." , j), ylab = paste("estimated K n." , k))
  abline( 0, 1)
  PI <- apply( post_d$K[,,k], 2, PI)
  # for( i in 1:sim_data_d$N){
  #   lines( rep( sim_data_d$K[i, j], 2), PI[,i])
  #  }
  }
}  
  
for (j in 1:sim_data_d$n_dimensions) {
  for(k in 1: sim_data_d$n_dimensions) {
  plot( sim_data_d$a[,j], apply( post_d$a_l[,,k], 2, mean), xlab = paste("simulated a n." , j), ylab = paste("estimated a n." , k))
  abline( 0, 1)
  # PI <- apply( post_d$g[,,k], 2, PI)
  # for( i in 1:sim_data_d$M){
  #   lines( rep( sim_data_d$g[i, j], 2), PI[,i])
  #  }
  }
}  

for (j in 1:sim_data_d$n_dimensions) {
  for(k in 1: sim_data_d$n_dimensions) {
  plot( sim_data_d$b[,j], apply( post_d$b_l[,,k], 2, mean), xlab = paste("simulated b n." , j), ylab = paste("estimated b n." , k))
  abline( 0, 1)
  # PI <- apply( post_d$b[,,k], 2, PI)
  # for( i in 1:sim_data_d$N){
  #   lines( rep( sim_data_d$b[i, j], 2), PI[,i])
  #  }
   }
}  

par( mfrow = c( 1, 1))

plot( sim_data_d$K[,2], apply( post_d$K3, 2, mean))
  abline( 0, 1)
  PI <- apply( post_d$K1, 2, PI)
  for( i in 1:sim_data_d$N){
    lines( rep( sim_data_d$K[i, 1], 2), PI[,i])
  }
  
#compare model with different n of dimensions
D <- c(1:3)
m_d <- list()
for (i in 1:length(D)) {
  dat <- list( D = D[i],
             N = sim_data$N , 
             L = sim_data$M , 
             Q = sim_data$M ,
             R = sim_data$M ,
             H = max(sim_data$HH),
             A = standardize(sim_data$A) , #standardized age
             SY= standardize(sim_data$SY), #standardized n of years of school
             OS= standardize(sim_data$OS),
             YS= standardize(sim_data$YS),
             AD= standardize(sim_data$Nad),
             HH= sim_data$HH, #integer for household
             am= sim_data$activity_matrix,
             C = sim_data$nact,
             Y_l = sim_data$Y , 
             Y_q = sim_data$Y , 
             Y_r = sim_data$Y
             )

m_d[i] <- cstan( file="~/Nextcloud/Project/Children_eco_knowledge/Children_eco_knowledge/models/model_code_dimensions_empty.stan" , data=dat , chains=3, cores=3, init = 0 )
}
compare(m_d[[1]], m_d[[2]], m_d[[3]])

###########################
#STRUCTURAL EQUATION MODEL#
###########################

precis(m_a)
precis(m_s)

#with no effect of SY
#    mean  sd 5.5% 94.5% n_eff Rhat4
# bA 0.42 0.1 0.26  0.59   107  1.01

#           mean   sd  5.5% 94.5% n_eff Rhat4
# bA        0.45 0.12  0.27  0.63    58  1.09
# bSY      -0.07 0.11 -0.24  0.10    77  1.03
# a_sy      0.00 0.08 -0.13  0.13  2271  1.00
# b_sy      0.53 0.09  0.39  0.66  2691  1.00
# sigma_sy  0.86 0.06  0.77  0.97  2169  1.00

#with effect of SY
#    mean   sd 5.5% 94.5% n_eff Rhat4
# bA  0.5 0.18 0.21   0.8   216  1.01

#          mean   sd  5.5% 94.5% n_eff Rhat4
# bA       0.47 0.17  0.19  0.76   313     1
# bSY      0.06 0.16 -0.19  0.32   191     1
# a_sy     0.01 0.17 -0.26  0.28  2533     1
# b_sy     0.19 -0.02  0.57  2081     1
# sigma_sy 1.01 0.15  0.80  1.26  1537     1

compare(m_a, m_s)

#one dimensional model, pooled individuals, pooled questions
dat <- list( N = sim_data$N ,              #n individuals
             L = sim_data$M ,              #n freelist items
             Q = 50,                      #n questionnaire items
             R = sim_data$M/2,             #n image recognition items
             H = max(sim_data$HH),         #n households
             A = standardize(sim_data$A) , #standardized age
             SY= standardize(sim_data$SY), #standardized n of years of school
             OS= standardize(sim_data$OS), #standardized n older brothers
             YS= standardize(sim_data$YS), #standardized n younger brothers
             AD= standardize(sim_data$Nad),#standardized n adults
             HH= sim_data$HH,              #integer for household
             am= sim_data$activity_matrix, #activities practiced
             C = sim_data$nact,            #total n activities
             Y_l = sim_data$Y ,                  #answers freelist
             Y_q = sim_data$Y[,1:50] ,           #answers questionnaire
             Y_r = sim_data$Y[,1:(sim_data$M/2)]  #answers picture recognition
             )

#complete model, 1 dimension
m <- cstan ( file = paste ( here(), "models/model_code.stan", sep = "/"), data=dat , chains=3, cores=3 )
m_mq <- cstan ( file = paste ( here(), "models/model_code_multi_qn.stan", sep = "/") , data=dat , chains=3, cores=3 )
m_mi <- cstan ( file = paste ( here(), "models/model_code_multi_ind.stan", sep = "/") , data=dat , chains=3, cores=3 )
compare(m, m_mq, m_mi)

