#extract posterior
post <- extract.samples(m)

#recover K
plot( biglist$K, apply( post$Ki, 2, mean))
abline( 0, 1)
PI <- apply( post$Ki, 2, PI)
for( i in 1:biglist$N){
  lines( rep( biglist$K[i], 2), PI[,i])
}

#recover g
plot( biglist$g, apply( post$g, 2, mean))
abline( 0, 1)
PI <- apply( post$g, 2, PI)
for( i in 1:biglist$M){
  lines( rep( biglist$g[i], 2), PI[,i])
}

#recover b
plot( biglist$b, apply( post$b, 2, mean))
abline( 0, 1)
PI <- apply( post$b, 2, PI)
for( i in 1:biglist$M){
  lines( rep( biglist$b[i], 2), PI[,i])
}

#recover l
plot( biglist$l, apply( post$l, 2, mean))
abline( 0, 1)
PI <- apply( post$l, 2, PI)
for( i in 1:biglist$M){
  lines( rep( biglist$l[i], 2), PI[,i])
}

#bA: evaluate model
  A_seq <- seq (from = -3, to = 3, length.out = 30)
  avg_K0 <- apply(post$K0, 1, mean)
  mu <- matrix(nrow = nrow(post$bA), ncol = length(A_seq)) #create empty matrix to store the fit model over the sequence of data
  for (i in 1:length(A_seq)) {
    mu[,i] <- avg_K0 + post$bA * A_seq[i]                 #calculate regression over the sequence of data A_seq, given the posterior
  }
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI)
  
  plot( standardize(biglist$A), biglist$K, xlab = "st_Age", ylab = "simulated_K")
  lines(A_seq, mu.mean)
  shade(mu.PI, A_seq)

#compare multiple values 
  #first, run compile stan model (model_code)
b_A <- c(0.2, 0.5, 0.8)
bA  <- c(NA, NA, NA)
bA_PI <- matrix(nrow = 3, ncol = 2)
 #NB this runs the model multiple times
for (i in 1:length(b_A)) {
  biglist <- sim_know(b_A = b_A[i])
  dat <- list( N = biglist$N , 
             M = biglist$M , 
             Y = biglist$Y , 
             A = standardize(biglist$A) , 
             SY= standardize(biglist$SY) 
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
  
#####
#multiple dimensions  
post_d <- extract.samples(m_d)

par( mfrow = c( biglist_d$n_dimensions, biglist_d$n_dimensions))

for (j in 1:biglist_d$n_dimensions) {
  for(k in 1: biglist_d$n_dimensions) {
  plot( biglist_d$K[,j], apply( post_d$aK[,,k], 2, mean))
  abline( 0, 1)
  PI <- apply( post_d$aK[,,k], 2, PI)
  for( i in 1:biglist_d$N){
    lines( rep( biglist_d$K[i, j], 2), PI[,i])
   }
  }
}  
  
for (j in 1:biglist_d$n_dimensions) {
  for(k in 1: biglist_d$n_dimensions) {
  plot( biglist_d$g[,j], apply( post_d$g[,,k], 2, mean), xlim = c(0, 10), ylim = c(0, 10))
  abline( 0, 1)
  PI <- apply( post_d$g[,,k], 2, PI)
  for( i in 1:biglist_d$N){
    lines( rep( biglist_d$g[i, j], 2), PI[,i])
   }
  }
}  

for (j in 1:biglist_d$n_dimensions) {
  for(k in 1: biglist_d$n_dimensions) {
  plot( biglist_d$b[,j], apply( post_d$b[,,k], 2, mean), xlim = c(-5, 5), ylim = c(-10, 10))
  abline( 0, 1)
  PI <- apply( post_d$b[,,k], 2, PI)
  for( i in 1:biglist_d$N){
    lines( rep( biglist_d$b[i, j], 2), PI[,i])
   }
  }
}  

par( mfrow = c( 1, 1))

plot( biglist_d$K[,2], apply( post_d$K3, 2, mean))
  abline( 0, 1)
  PI <- apply( post_d$K1, 2, PI)
  for( i in 1:biglist_d$N){
    lines( rep( biglist_d$K[i, 1], 2), PI[,i])
  }
  
plot( biglist_d$g[,1], apply( post_d$g_3, 2, mean))
abline( 0, 1)
PI <- apply( post$g, 2, PI)
for( i in 1:biglist$M){
  lines( rep( biglist$g[i], 2), PI[,i])
}