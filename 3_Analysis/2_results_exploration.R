###########
#FUNCTIONS#
###########
#colors
d$sex_col <- ifelse(d$S == "m", "darkblue", "darkred")#assign color to each sex


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


######################
#ADDING OTHER FACTORS#
######################

############
#ACTIVITIES#
############

plot (precis (m_act, 3, pars = "aAM"))
axis(2, at=10:1, labels=colnames(d$am), par(las=1))


post_a <- extract.samples(m_act) #extract samples

plot_age_sex(post = post_a, d = d, dot_col = d$sex_col)

##########
#FAMILIES#
##########
plot(precis(m_fam, 3, pars = "aH"))

post_f <- extract.samples(m_fam)

plot_age_sex(post = post_f, d = d, dot_col = as.character(d$HH))


###########
#SCHOOLING#
###########
precis(m_sch, 3, pars = "bSY")

#ordered schooling
post_sc <- extract.samples(m_sch) #extract samples
post <- post_sc
sch_eff <- apply(post$delta_j, 1, cumsum)
#aK[i,j] + aS[S[i],j] + bA[S[i],j]*A[i] + bSY[S[i],j] * sum (delta_js[ 1 : SY[i] ]  ) ; 
plot(1:nrow(sch_eff), mean(post$bSY[,1,]) * sch_eff[,1], type = "l", 
    xlab = "Age", ylab = "School effect on knowledge" ,
    ylim = c(-1, 1), xaxt='n')
axis(1, c("no_school", "some_el", "lot_el", "some_high", "lot_high"), at = c(1:5))
for (i in 1:50) {
  lines(1:nrow(sch_eff),  post$bSY[i,1,] * sch_eff[,i], type = "l", col = col.alpha( 'darkblue', alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(sch_eff),  post$bSY[i,2,] * sch_eff[,i], type = "l", col = col.alpha( 'darkred', alpha = 0.1))
}

#plot effect of schooling
post_s <- extract.samples(m_sch) #extract samples
dot_col <- d$sex_col
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


#######################
#ORDERED CATEGORICAL A#
#######################
#extract samples
post_ma <- extract.samples(m_ord_mina)

post <- post_ma
#INDIVIDUALS
############
#plot knowledge estimation by age
Ks <- apply(post$K, 2, mean)
plot(d$A [ d$A <= 50 ], Ks, xlab = "Age", ylab = "Knowledge" )

aKs <- apply(post$aK, 2, mean)
plot( Ks, aKs)
plot(d$A [ d$A <= 50 ], aKs, xlab = "Age", ylab = "Individual effect on knowledge")

#explore age effects
plot (precis(m_ord_mina, 3, pars = "delta_j"))

year_eff <- apply(post$delta_j, 1, cumsum)
plot(d$A, apply(post$K, 2, mean), xlab = "Age", ylab = "Knowledge", col = dot_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  mean(post$mA) + mean(post$bA) * year_eff[,i], type = "l", col = col.alpha( 'darkblue', alpha = 0.1))
}

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



#ITEMS
######
#freelist
a_ls <- apply(post$a_l, 2, mean)
b_ls <- apply(post$b_l, 2, mean)

curve(inv_logit(a_ls[1] * ( x - b_ls[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge in freelists", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls)){
  curve(inv_logit(a_ls[i] * ( x - b_ls[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks, rep(0.5, length(Ks)),col = col.alpha("cornflowerblue", 0.7))  

#questions
a_qs <- apply(post$a_q, 2, mean)
b_qs <- apply(post$b_q, 2, mean)

curve(inv_logit(a_qs[1] * ( x - b_qs[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge in questions", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_qs)){
  curve(inv_logit(a_qs[i] * ( x - b_qs[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks, rep(0.5, length(Ks)),col = col.alpha("cornflowerblue", 0.7))  

#check image recognition
a_rs <- apply(post$a_r, 2, mean)
b_rs <- apply(post$b_r, 2, mean)

curve(inv_logit(a_rs[1] * ( x - b_rs[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge in image recognition", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_rs)){
  curve(inv_logit(a_rs[i] * ( x - b_rs[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks, rep(0.5, length(Ks)),col = col.alpha("cornflowerblue", 0.7))  

############
#DIMENSIONS#
############
post_d <- extract.samples(m_d3)

#check the correlation between dimensions
par( mfrow = c( 1,3))
#knowledge
plot(apply( post_d$K[,,1], 2, mean), apply( post_d$K[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2")
plot(apply( post_d$K[,,1], 2, mean), apply( post_d$K[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3")
plot(apply( post_d$K[,,2], 2, mean), apply( post_d$K[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3")
#discrimination
plot(apply( post_d$a_l[,,1], 2, mean), apply( post_d$a_l[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2")
plot(apply( post_d$a_l[,,1], 2, mean), apply( post_d$a_l[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3")
plot(apply( post_d$a_l[,,2], 2, mean), apply( post_d$a_l[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3")
#difficulty
plot(apply( post_d$b_l[,,1], 2, mean), apply( post_d$b_l[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2")
plot(apply( post_d$b_l[,,1], 2, mean), apply( post_d$b_l[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3")
plot(apply( post_d$b_l[,,2], 2, mean), apply( post_d$b_l[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3")

#check effect of age
precis(m_d[[1]], pars = "bA")
precis(m_d[[2]], pars = "bA")
precis(m_d[[3]], pars = "bA")


#check knowledge and questions parameters
#1st dimension
Ks_1 <- apply(post_d$K[,,1], 2, mean)
a_ls_1 <- apply(post_d$a_l[,,1], 2, mean)
b_ls_1 <- apply(post_d$b_l[,,1], 2, mean)
curve(inv_logit(a_ls_1[1] * ( x - b_ls_1[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 1", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls_1)){
  curve(inv_logit(a_ls_1[i] * ( x - b_ls_1[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks_1, rep(0.5, length(Ks_1)),col = col.alpha("cornflowerblue", 0.7))  

#2nd dimension
Ks_2 <- apply(post_d$K[,,2], 2, mean)
a_ls_2 <- apply(post_d$a_l[,,2], 2, mean)
b_ls_2 <- apply(post_d$b_l[,,2], 2, mean)
curve(inv_logit(a_ls_2[1] * ( x - b_ls_2[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 2", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls_2)){
  curve(inv_logit(a_ls_2[i] * ( x - b_ls_2[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks_2, rep(0.5, length(Ks_2)),col = col.alpha("cornflowerblue", 0.7))  

#3rd dimension
Ks_3 <- apply(post_d$K[,,3], 2, mean)
a_ls_3 <- apply(post_d$a_l[,,3], 2, mean)
b_ls_3 <- apply(post_d$b_l[,,3], 2, mean)
curve(inv_logit(a_ls_3[1] * ( x - b_ls_3[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 3", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls_3)){
  curve(inv_logit(a_ls_3[i] * ( x - b_ls_3[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks_3, rep(0.5, length(Ks_1)),col = col.alpha("cornflowerblue", 0.7))  

par( mfrow = c( 1, 1))

#coloring by type of question
unique (all_items$type)

d$color_l <- ifelse( is.na(d$type_l), "yellow",
             ifelse( d$type_l  == "N", "darkorange",
             ifelse( d$type_l  == "S", "darkblue",
             ifelse( d$type_l  == "W", "darkred",
             ifelse( d$type_l  == "D", "orchid",
             ifelse( d$type_l  == "M", "darkgreen", 
             NA))))))

#1st dimension
Ks_1 <- apply(post_d$K[,,1], 2, mean)
a_ls_1 <- apply(post_d$a_l[,,1], 2, mean)
b_ls_1 <- apply(post_d$b_l[,,1], 2, mean)
curve(inv_logit(a_ls_1[1] * ( x - b_ls_1[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 1", ylab = "p correct answer",
      col = col.alpha( d$color_l, 0.2))
for(i in 1: length(a_ls_1)){
  curve(inv_logit(a_ls_1[i] * ( x - b_ls_1[i])), 
        col = col.alpha(d$color_l[i], 0.2), add = TRUE)
}
points(Ks_1, rep(0.5, length(Ks_1)), col = col.alpha("cornflowerblue", 0.7))  

#2nd dimension
Ks_2 <- apply(post_d$K[,,2], 2, mean)
a_ls_2 <- apply(post_d$a_l[,,2], 2, mean)
b_ls_2 <- apply(post_d$b_l[,,2], 2, mean)
curve(inv_logit(a_ls_2[1] * ( x - b_ls_2[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 2", ylab = "p correct answer",
      col = col.alpha(d$color_l, 0.2))
for(i in 1: length(a_ls_2)){
  curve(inv_logit(a_ls_2[i] * ( x - b_ls_2[i])), 
        col = col.alpha(d$color_l[i], 0.2), add = TRUE)
}
points(Ks_2, rep(0.5, length(Ks_2)),col = col.alpha("cornflowerblue", 0.7))  

#3rd dimension
Ks_3 <- apply(post_d$K[,,3], 2, mean)
a_ls_3 <- apply(post_d$a_l[,,3], 2, mean)
b_ls_3 <- apply(post_d$b_l[,,3], 2, mean)
curve(inv_logit(a_ls_3[1] * ( x - b_ls_3[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 3", ylab = "p correct answer",
      col = col.alpha(d$color_l, 0.2))
for(i in 1: length(a_ls_3)){
  curve(inv_logit(a_ls_3[i] * ( x - b_ls_3[i])), 
        col = col.alpha(d$color_l[i], 0.2), add = TRUE)
}
points(Ks_3, rep(0.5, length(Ks_3)),col = col.alpha("cornflowerblue", 0.7))  
