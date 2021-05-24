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
plot(d$A[ d$A <= 50 ], apply(post$K, 2, mean), xlab = "Age", ylab = "Knowledge", col = dot_col )
for (i in 1:50) {
  lines(1:nrow(year_eff_1),  mean(post$mA) + mean(post$bA[,,1]) * year_eff_1[,i], type = "l", col = col.alpha( 'darkblue', alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff_2),  mean(post$mA) + mean(post$bA[,,2]) * year_eff_2[,i], type = "l", col = col.alpha( 'darkred', alpha = 0.1))
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

