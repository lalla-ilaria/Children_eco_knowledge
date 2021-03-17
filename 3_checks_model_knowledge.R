#exploration of the prior
curve( inv_logit(rlnorm(1, 0, 0.5) * ( x - rnorm(1, 0, 1) ) ), xlim = c(-4, 4) , ylim = c(0, 1) )
for(i in 1:20)curve( inv_logit(rlnorm(1, 0, 0.5) * ( x - rnorm(1, 0, 1) ) ), add = TRUE)

curve( (rnorm(1, 0, 1) + rnorm(1, 0, 0.5) + abs(rnorm (1, 0, 0.5)) * x + abs(rnorm (1, 0, 0.3)) * (x/3)  ), xlim = c(-5, 5) , ylim = c(-5, 5), ylab = "K" )
for(i in 1:20) curve( (rnorm(1, 0, 1) +rnorm(1, 0, 0.5) + abs(rnorm (1, 0, 0.5)) * x + abs(rnorm (1, 0, 0.3)) * (x/3) ), add = TRUE, ylab = NULL)

#extract posterior
post <- extract.samples(m_mi)

#recover K
plot( biglist$K, apply( post$K, 2, mean))
abline( 0, 1)
PI <- apply( post$K, 2, PI)
for( i in 1:biglist$N){
  lines( rep( biglist$K[i], 2), PI[,i])
}

#recover g
plot( biglist$a, apply( post$a_l, 2, mean))
abline( 0, 1)
# PI <- apply( post$a_l, 2, PI)
# for( i in 1:biglist$M){
#   lines( rep( biglist$a[i], 2), PI[,i])
# }

#recover b
plot( biglist$b[1:50], apply( post$b_r, 2, mean))
abline( 0, 1)
# PI <- apply( post$b, 2, PI)
# for( i in 1:biglist$M){
#   lines( rep( biglist$b[i], 2), PI[,i])
# }

#recover l
plot( biglist$l, apply( post$l, 2, mean))
abline( 0, 1)
PI <- apply( post$l, 2, PI)
for( i in 1:biglist$M){
  lines( rep( biglist$l[i], 2), PI[,i])
}

#evaluate A estimation
plot( biglist$A, apply( post$Ar, 2, mean))
abline( 0, 1)
PI <- apply( post$Ar, 2, PI)
for( i in 1:biglist$N){
  lines( rep( biglist$A[i], 2), PI[,i])
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
  
  plot( standardize(biglist$A), biglist$K, xlab = "st_Age", ylab = "simulated_K")
  lines(A_seq, mu.mean)
  shade(mu.PI, A_seq)

#compare multiple values 
b_A <- c(0.2, 0.5, 0.8)
bA  <- c(NA, NA, NA)
bA_PI <- matrix(nrow = 3, ncol = 2)
 #NB this runs the model multiple times
for (i in 1:length(b_A)) {
  biglist <- sim_know(b_A = b_A[i])
  dat <- list( N = biglist$N , 
             M = biglist$M , 
             H = max(biglist$HH),
             Y = biglist$Y , 
             A = standardize(biglist$A) , #standardized age
             SY= standardize(biglist$SY), #standardized n of years of school
             OS= standardize(biglist$OS),
             YS= standardize(biglist$YS),
             AD= standardize(biglist$Nad),
             HH= biglist$HH, #integer for household
             am= biglist$activity_matrix,
             C = biglist$nact
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
  


#compare models with multilevel individuals vs questions
#want to see if the PI around K (but also a and b) is reduced more with pooling over individuals or questions
diffs_K <- list()
diffs_a <- list()
diffs_b <- list()
mean_K <- list()
mean_a <- list()
mean_b <- list()

  biglist <- sim_know()
#data
#####
#one dimensional model, pooled individuals, pooled questions
dat <- list( N = biglist$N , #n individuals
             L = biglist$M , #n freelist items
             Q = 50,         #n questionnaire items
             R = biglist$M/2,#n image recognition items
             H = max(biglist$HH), #n households
             A = standardize(biglist$A) , #standardized age
             SY= standardize(biglist$SY), #standardized n of years of school
             OS= standardize(biglist$OS), #standardized n older brothers
             YS= standardize(biglist$YS), #standardized n younger brothers
             AD= standardize(biglist$Nad),#standardized n adults
             HH= biglist$HH, #integer for household
             am= biglist$activity_matrix, #activities practiced
             C = biglist$nact, #total n activities
             Y_l = biglist$Y ,                  #answers freelist
             Y_q = biglist$Y[,1:50] ,           #answers questionnaire
             Y_r = biglist$Y[,1:(biglist$M/2)]  #answers picture recognition
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
 plot(biglist$K, mean_K[[1]], col = "red")
 points(biglist$K, mean_K[[2]], col = "blue")
 points(biglist$K, mean_K[[3]], col = "green")
#same for b and a
 PIs <- list()
 for(i in 1:3) {
   PIs[[i]] <- apply( post[[i]]$a_l, 2, PI)
   diffs_a[[i]] <- (mean(diff(PIs[[i]])))
   mean_a[[i]] <- apply( post[[i]]$a_l, 2, mean)
 }
 plot(biglist$a, mean_a[[1]], col = "red")
 points(biglist$a, mean_a[[2]], col = "blue")
 points(biglist$a, mean_a[[3]], col = "green")
 PIs <- list()
 for(i in 1:3) {
   PIs[[i]] <- apply( post[[i]]$b_l, 2, PI)
   diffs_b[[i]] <- (mean(diff(PIs[[i]])))
   mean_b[[i]] <- apply( post[[i]]$b_l, 2, mean)
  }




#####
#multiple dimensions  
post_d <- extract.samples(m_d)

par( mfrow = c( biglist_d$n_dimensions, biglist_d$n_dimensions))

for (j in 1:biglist_d$n_dimensions) {
  for(k in 1: biglist_d$n_dimensions) {
  plot( biglist_d$K[,j], apply( post_d$K[,,k], 2, mean), xlab = paste("simulated K n." , j), ylab = paste("estimated K n." , k))
  abline( 0, 1)
  PI <- apply( post_d$K[,,k], 2, PI)
  # for( i in 1:biglist_d$N){
  #   lines( rep( biglist_d$K[i, j], 2), PI[,i])
  #  }
  }
}  
  
for (j in 1:biglist_d$n_dimensions) {
  for(k in 1: biglist_d$n_dimensions) {
  plot( biglist_d$a[,j], apply( post_d$a_l[,,k], 2, mean), xlab = paste("simulated a n." , j), ylab = paste("estimated a n." , k))
  abline( 0, 1)
  # PI <- apply( post_d$g[,,k], 2, PI)
  # for( i in 1:biglist_d$M){
  #   lines( rep( biglist_d$g[i, j], 2), PI[,i])
  #  }
  }
}  

for (j in 1:biglist_d$n_dimensions) {
  for(k in 1: biglist_d$n_dimensions) {
  plot( biglist_d$b[,j], apply( post_d$b_l[,,k], 2, mean), xlab = paste("simulated b n." , j), ylab = paste("estimated b n." , k))
  abline( 0, 1)
  # PI <- apply( post_d$b[,,k], 2, PI)
  # for( i in 1:biglist_d$N){
  #   lines( rep( biglist_d$b[i, j], 2), PI[,i])
  #  }
   }
}  

par( mfrow = c( 1, 1))

plot( biglist_d$K[,2], apply( post_d$K3, 2, mean))
  abline( 0, 1)
  PI <- apply( post_d$K1, 2, PI)
  for( i in 1:biglist_d$N){
    lines( rep( biglist_d$K[i, 1], 2), PI[,i])
  }
  
#compare model with different n of dimensions
D <- c(1:3)
m_d <- list()
for (i in 1:length(D)) {
  dat <- list( N = biglist_d$N , 
             M = biglist_d$M , 
             Y = biglist_d$Y , 
             A = standardize(biglist_d$A) , 
             SY= standardize(biglist_d$SY),
             am = biglist_d$activity_matrix,
             C = biglist_d$nact,
             D = D[i]
             )

m_d[i] <- cstan( model_code = model_code_d , data = dat , chains = 3, cores = 3) # ,control = list(adapt_delta = 0.9, max_treedepth = 15) use cstan cause it uses commandstan
}
compare(m_d[[1]], m_d[[2]], m_d[[3]])