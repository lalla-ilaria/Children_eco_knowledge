library(scales)
########
#COLORS#
########
#colors
boycol <- "navyblue"
girlcol <- "red3"
d$sex_col <- ifelse(d$S == "m", boycol, girlcol)#assign color to each sex
d$color_l <- ifelse( is.na(d$type_l), "slategray1",
             ifelse( d$type_l  == "N", "orange1",
             ifelse( d$type_l  == "S", "deepskyblue3",
             ifelse( d$type_l  == "W", "firebrick2",
             ifelse( d$type_l  == "D", "deeppink3",
             ifelse( d$type_l  == "M", "limegreen", 
             NA))))))

#######################
#ORDERED CATEGORICAL A#
#######################
#extract samples
post_agedt <- extract.samples(m_agedt)
post <- post_age

#INDIVIDUALS
############
#plot knowledge estimation by age
plot( apply(post$K, 2, mean), apply(post$aK, 2, mean))


#explore age effects
#plot (precis(m_age, 3, pars = "delta_j"))
plot(apply(post$delta_j, 2, mean), 1:ncol(post$delta_j), 
     xlab = "Age specific increase", ylab = "ages")
for (i in 1:ncol(post$delta_j)) lines(apply(post$delta_j, 2, PI)[,i], rep(i, each = 2))

#mA + aK[i,d] + bA[S[i], d] * sum (delta_j[ 1 : A[i] ] ) ; 
year_eff <- apply(post$delta_j, 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post$K, 2, mean), xlim = c(0,27),
     xlab = "Age", ylab = "Knowledge",  
     pch = 19, col = alpha( d$sex_col, 0.6 ) )
for (i in 1:80) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,1,] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:80) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,2,] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
lines(1:nrow(year_eff_1),  mean(post$mA) + mean(post$bA[,1,]) * apply(year_eff, 1, mean), type = "l", col = col.alpha( boycol, alpha = 0.7))
lines(1:nrow(year_eff_2),  mean(post$mA) + mean(post$bA[,2,]) * apply(year_eff, 1, mean), type = "l", col = col.alpha( girlcol, alpha = 0.7))


#diff deltas
year_eff_1 <- apply(post$delta_j[,,1], 1, cumsum)
year_eff_2 <- apply(post$delta_j[,,2], 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post$K, 2, mean), 
     xlab = "Age", ylab = "Knowledge",  
     pch = 19, col = alpha( d$sex_col, 0.6 ) )
for (i in 1:80) {
  lines(1:nrow(year_eff_1),  post$mA[i] + post$bA[i,1,] * year_eff_1[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:80) {
  lines(1:nrow(year_eff_2),  post$mA[i] + post$bA[i,2,] * year_eff_2[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
lines(1:nrow(year_eff_1),  mean(post$mA) + mean(post$bA[,1,]) * apply(year_eff_1, 1, mean), type = "l", col = col.alpha( boycol, alpha = 0.7))
lines(1:nrow(year_eff_2),  mean(post$mA) + mean(post$bA[,2,]) * apply(year_eff_2, 1, mean), type = "l", col = col.alpha( girlcol, alpha = 0.7))



#ITEMS
######
#freelist
a_ls <- apply(post$a_l, 2, mean)
b_ls <- apply(post$b_l, 2, mean)

curve(inv_logit(a_ls[1] * ( x - b_ls[1])), 
      xlim = c(-10, 5), ylim = c(0, 1), 
      xlab = "knowledge in freelists", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls)){
  curve(inv_logit(a_ls[i] * ( x - b_ls[i])), 
        col = col.alpha(d$color_l[i], 
                        alpha = ifelse(is.na(d$type_l[i]), 0.2, 
                                ifelse( d$type_l[i] == "S",  0.2, 0.2))), add = TRUE)
}
points(apply(post$K, 2, mean), rep(0.5, 93),
       pch = 19, col = col.alpha("cornflowerblue", 0.4))  

#questions
a_qs <- apply(post$a_q, 2, mean)
b_qs <- apply(post$b_q, 2, mean)

curve(inv_logit(a_qs[1] * ( x - b_qs[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge in questions", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.6))
for(i in 1: length(a_qs)){
  curve(inv_logit(a_qs[i] * ( x - b_qs[i])), col = col.alpha("lightblue", 0.8), add = TRUE)
}
points(apply(post$K, 2, mean), rep(0.5, 93),
       pch = 19, col = col.alpha("cornflowerblue", 0.4))  

#check image recognition
a_rs <- apply(post$a_r, 2, mean)
b_rs <- apply(post$b_r, 2, mean)

curve(inv_logit(a_rs[1] * ( x - b_rs[1])), 
      xlim = c(-11, 4), ylim = c(0, 1), 
      xlab = "knowledge in image recognition", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.6))
for(i in 1: length(a_rs)){
  curve(inv_logit(a_rs[i] * ( x - b_rs[i])), col = col.alpha("lightblue", 0.7), add = TRUE)
}
points(apply(post$K, 2, mean), rep(0.5, 93),
       pch = 19, col = col.alpha("cornflowerblue", 0.4))  



######################
#ADDING OTHER FACTORS#
######################

############
#ACTIVITIES#
############
post_act <- extract.samples(m_act)
post <- post_act

#check activity effect
plot (precis (m_act, 3, pars = "aAM"))
axis(2, at=10:1, labels=colnames(d$am), par(las=1))
# #as violin plot
# act <- as.data.frame(post$aAM[,,1])
# colnames(act) <- act_names
# act <- act %>% gather(activity, effect,1:10)
# ggplot(act, aes ( x = effect,
#              y = activity))+
#   geom_vline(xintercept =0, col = "grey")+
#   geom_violin()+
#   theme_classic()
#as ridgeplot
act <- split(post$aAM[,,1], rep(1:ncol(post$aAM[,,1]), each = nrow(post$aAM[,,1])))
names(act) <- act_names
act <- act[order(sapply(act, mean))]
ridgeplot( act,
           step = 1.1,
           col = "cornflowerblue",
           fill = col.alpha("cornflowerblue", 0.2))
abline(v = 0, col = col.alpha("grey", 0.2))


plot(x = ac$mu, 
     y = 1:10, 
     xlim = c(-1.6, 1.5),
     cex.lab=1.8, 
     cex.axis=1.8, 
     pch = 19, 
     xlab = "Effect of activities", 
     ylab = "", 
     yaxt='n',
     family = "A",
     col = ac$col_0)
title( "c - Activity effects", adj = 0, cex.main = 1.8, family = "A")
axis(2, ac$names, at = c(1:10), las = 1, cex.axis = 1.2, family = "A")
abline(v = 0, col = "gray")
for (i in 1:10) lines(c(ac$lim5.5[i],ac$lim94.5[i]) , rep(i, each = 2), col = ac$col_0[i], lwd = 1.5)

#or
plot(apply(post$aAM, 2, mean), 1:10, xlim = c(-1.6, 1.5),
     xlab = "Activity effect", ylab = "Activities", yaxt='n')
axis(2, colnames(d$amh), at = c(1:10), las = 1)
for (i in 1:10) lines(apply(post$aAM, 2, PI)[,i], rep(i, each = 2))
abline(v = 0)

#############################################################################
#explore participation to activities
apply(d$amh, 2, sum)
plot(jitter(d$amh[,6]), jitter(d$amh[,8]))
#neeed to check if people who do certain activities name things that have to do with them


#explore age effects
#plot (precis(m_age, 3, pars = "delta_j"))
plot(apply(post$delta_j, 2, mean), 1:ncol(post$delta_j), 
     xlab = "Age specific increase", ylab = "ages")
for (i in 1:27) lines(apply(post$delta_j, 2, PI)[,i], rep(i, each = 2))

#age-sex specific knowledge with activities
# mA + aK[i,d] + bA[S[i], d] * sum (delta_j[ 1 : A[i] ] ) +dot_product( aAM[,d], AM[i]);
year_eff <- apply(post$delta_j, 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post$K, 2, mean), 
     xlab = "Age", ylab = "Knowledge", 
     pch = 19, col = alpha( d$sex_col, 0.6 ) )
for (i in 1:80) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,1,] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:80) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,2,] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
lines(1:nrow(year_eff_1),  mean(post$mA) + mean(post$bA[,1,]) * apply(year_eff, 1, mean), type = "l", col = col.alpha( boycol, alpha = 0.7))
lines(1:nrow(year_eff_2),  mean(post$mA) + mean(post$bA[,2,]) * apply(year_eff, 1, mean), type = "l", col = col.alpha( girlcol, alpha = 0.7))


###############
#contrasts sex#
###############
plot(NULL, xlim = c(-2, 4), ylim = c(0.5,2.5), yaxt='n')
points(mean(post_age$bA[,1,] - post_age$bA[,2,]), 1)
points(mean(post_act$bA[,1,] - post_act$bA[,2,]), 2)
lines(PI(post_age$bA[,1,] - post_age$bA[,2,]), c(1,1))
lines(PI(post_act$bA[,1,] - post_act$bA[,2,]), c(2,2))
abline(v = 0)
axis(2, c("age", "act" ), at = c(1:2), las = 1)


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
post_sch <- extract.samples(m_sch) #extract samples
post <- post_sch
sch_eff <- apply(post$delta_j, 1, cumsum)

#school effect
#mA +aK[i,d] + bA[S[i], d] * sum (delta_j[ 1 : A[i] ] ) + bSY[S[i],d] * sum (delta_js[ 1 : SY[i] ] ) 

#with effect of concluding only a certain part of the school career
plot(1:nrow(sch_eff), mean(post$bSY[,1,]) * sch_eff[,1], type = "l", 
    xlab = "Age", ylab = "School effect on knowledge" ,
    ylim = c(-1, 1), xaxt='n')
axis(1, c("no_school", "some_el", "lot_el", "some_high", "lot_high"), at = c(1:5))
for (i in 1:50) {
  lines(1:nrow(sch_eff),  post$bSY[i,1,] * sch_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(sch_eff),  post$bSY[i,2,] * sch_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}



############
#DIMENSIONS#
############
waics <- compare( m_d[[2]], m_d[[3]], m_d[[4]])
waicsd <- compare( m_dd[[2]], m_dd[[3]], m_dd[[4]])
post_2 <- extract.samples(m_d[[2]])
post_3 <- extract.samples(m_d[[3]])
post_4 <- extract.samples(m_d[[4]])

postd_2 <- extract.samples(m_dd[[2]])
postd_3 <- extract.samples(m_dd[[3]])
postd_4 <- extract.samples(m_dd[[4]])

post_d <- post_3

#check the correlation between dimensions
par( mfrow = c( 1,3))
#knowledge
plot(apply( post_d$K[,,1], 2, mean), apply(post_d$aK, 2, mean),
     xlab = "K 1", ylab = "aK",col = d$sex_col)
plot(apply( post_d$K[,,2], 2, mean), apply(post_d$aK, 2, mean),
     xlab = "K 2", ylab = "aK",col = d$sex_col)
plot(apply( post_d$K[,,3], 2, mean), apply(post_d$aK, 2, mean),
     xlab = "K 3", ylab = "aK",col = d$sex_col)

plot(apply( post_d$K[,,1], 2, mean), apply( post_d$K[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2",col = d$sex_col)
plot(apply( post_d$K[,,1], 2, mean), apply( post_d$K[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3",col = d$sex_col)
plot(apply( post_d$K[,,2], 2, mean), apply( post_d$K[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3",col = d$sex_col)
#discrimination
plot(apply( post_d$a_l[,,1], 2, mean), apply( post_d$a_l[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2", col = d$color_l)
plot(apply( post_d$a_l[,,1], 2, mean), apply( post_d$a_l[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3",col = d$color_l)
plot(apply( post_d$a_l[,,2], 2, mean), apply( post_d$a_l[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3",col = d$color_l)
#difficulty
plot(apply( post_d$b_l[,,1], 2, mean), apply( post_d$b_l[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2",col = d$color_l)
plot(apply( post_d$b_l[,,1], 2, mean), apply( post_d$b_l[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3",col = d$color_l)
plot(apply( post_d$b_l[,,2], 2, mean), apply( post_d$b_l[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3",col = d$color_l)

#check effect of age
precis(m_d[[1]], pars = "bA")
precis(m_d[[2]], pars = "bA")
precis(m_d[[3]], pars = "bA")

############
#Age effect#
#dimension specific deltas
year_eff <- apply(post_d$delta_j[,,1], 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,1], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,1] + post_d$bA[i,1,1] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,1] + post_d$bA[i,2,1] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
year_eff <- apply(post_d$delta_j[,,2], 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,2], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,2] + post_d$bA[i,1,2] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,2] + post_d$bA[i,2,2] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
year_eff <- apply(post_d$delta_j[,,3], 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,3], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,3] + post_d$bA[i,1,3] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,3] + post_d$bA[i,2,3] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}


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
        col = col.alpha(d$color_l[i], 
                        alpha = ifelse(is.na(d$type_l[i]), 0.2, 
                                       ifelse( d$type_l[i] == "M",  0.8, 0.2))), add = TRUE)
  
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
        col = col.alpha(d$color_l[i], 
                        alpha = ifelse(is.na(d$type_l[i]), 0.2, 
                                       ifelse( d$type_l[i] == "M",  0.8, 0.2))), add = TRUE)
  
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
        col = col.alpha(d$color_l[i], 
                        alpha = ifelse(is.na(d$type_l[i]), 0.2, 
                                       ifelse( d$type_l[i] == "M",  0.8, 0.2))), add = TRUE)
  
}
points(Ks_3, rep(0.5, length(Ks_3)),col = col.alpha("cornflowerblue", 0.7))  


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
  lines(A_seq_real, mus1.mean, col = boycol)
  shade(mus1.PI, A_seq_real, col = col.alpha(boycol, 0.2))
  lines(A_seq_real, mus2.mean, col = girlcol)
  shade(mus2.PI, A_seq_real, col =  col.alpha(girlcol, 0.2))
  
}


#4d
par( mfrow = c( 1,4))
#knowledge
plot(apply( post_d$K[,,1], 2, mean), apply(post_d$aK, 2, mean),
     xlab = "K 1", ylab = "aK",col = d$sex_col)
plot(apply( post_d$K[,,2], 2, mean), apply(post_d$aK, 2, mean),
     xlab = "K 2", ylab = "aK",col = d$sex_col)
plot(apply( post_d$K[,,3], 2, mean), apply(post_d$aK, 2, mean),
     xlab = "K 3", ylab = "aK",col = d$sex_col)
plot(apply( post_d$K[,,4], 2, mean), apply(post_d$aK, 2, mean),
     xlab = "K 4", ylab = "aK",col = d$sex_col)

plot(apply( post_d$K[,,1], 2, mean), apply( post_d$K[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2",col = d$sex_col)
plot(apply( post_d$K[,,1], 2, mean), apply( post_d$K[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3",col = d$sex_col)
plot(apply( post_d$K[,,2], 2, mean), apply( post_d$K[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3",col = d$sex_col)
plot(apply( post_d$K[,,1], 2, mean), apply( post_d$K[,,4], 2, mean),
     xlab = "K 1", ylab = "K 4",col = d$sex_col)
plot(apply( post_d$K[,,4], 2, mean), apply( post_d$K[,,3], 2, mean),
     xlab = "K 4", ylab = "K 3",col = d$sex_col)
plot(apply( post_d$K[,,2], 2, mean), apply( post_d$K[,,4], 2, mean),
     xlab = "K 2", ylab = "K 4",col = d$sex_col)


#dimension specific deltas
year_eff <- apply(post_d$delta_j[,,1], 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,1], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,1] + post_d$bA[i,1,1] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,1] + post_d$bA[i,2,1] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
year_eff <- apply(post_d$delta_j[,,2], 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,2], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,2] + post_d$bA[i,1,2] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,2] + post_d$bA[i,2,2] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
year_eff <- apply(post_d$delta_j[,,3], 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,3], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,3] + post_d$bA[i,1,3] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,3] + post_d$bA[i,2,3] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}
year_eff <- apply(post_d$delta_j[,,4], 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post_d$K[,,4], 2, mean), 
     xlab = "Age", ylab = "Knowledge", col = d$sex_col )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,4] + post_d$bA[i,1,4] * year_eff[,i], type = "l", col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post_d$mA[i,4] + post_d$bA[i,2,4] * year_eff[,i], type = "l", col = col.alpha( girlcol, alpha = 0.1))
}

b_ls <- apply(post_age$b_l, 2, mean)
b_ls_1 <- apply(post_d$b_l[,,1], 2, mean)
b_ls_2 <- apply(post_d$b_l[,,2], 2, mean)
b_ls_3 <- apply(post_d$b_l[,,3], 2, mean)
print("one dimension model")
colnames(d$Y_l)[which(b_ls <=max(sort(b_ls)[1:10]))]
colnames(d$Y_l)[which(b_ls >=min(sort(b_ls, decreasing = T)[1:10]))]
print("three dimensions model_2")
colnames(d$Y_l)[which(b_ls_2 <=max(sort(b_ls_2)[1:10]))]
colnames(d$Y_l)[which(b_ls_2 >=min(sort(b_ls_2, decreasing = T)[1:30]))]
print("three dimensions model_1")
colnames(d$Y_l)[which(b_ls_1 <=max(sort(b_ls_1)[1:10]))]
colnames(d$Y_l)[which(b_ls_1 >=min(sort(b_ls_1, decreasing = T)[1:30]))]
print("three dimensions model_3")
colnames(d$Y_l)[which(b_ls_3 <=max(sort(b_ls_3)[1:10]))]
colnames(d$Y_l)[which(b_ls_3 >=min(sort(b_ls_3, decreasing = T)[1:30]))]

Ks <- apply(post_age$K, 2, mean)
Ks_1 <- apply(post_d$K[,,1], 2, mean)
Ks_2 <- apply(post_d$K[,,2], 2, mean)
Ks_3 <- apply(post_d$K[,,3], 2, mean)
best_all <- rownames(d$Y_l)[which(Ks >=min(sort(Ks, decreasing = T)[1:10]))]
best_1 <- rownames(d$Y_l)[which(Ks_2 >=min(sort(Ks_2, decreasing = T)[1:10]))]
best_2 <- rownames(d$Y_l)[which(Ks_1 >=min(sort(Ks_1, decreasing = T)[1:10]))]
best_3 <- rownames(d$Y_l)[which(Ks_3 >=min(sort(Ks_3, decreasing = T)[1:10]))]

rare_w <- which(apply(d$Y_l, 2, sum) <=1)
rare_w_all <- NA
for (i in 1:10){
  rare_w_all <- append( rare_w_all, names(  rare_w[ which (names(rare_w) %in% names(which(d$Y_l[which( rownames(d$Y_l) == best_all[i]),]>=1)))]))}
rw_all <- data.frame(rare_w_all, type = rep(NA, length(rare_w_all)), dim = rep("d_all", length(rare_w_all)))
rw_all <- rw_all[-is.na(rw_all$rare_w_all),]
for ( i in 1: nrow(rw_all)) {
  rw_all$type[i] <- d$type_l[which( colnames(d$Y_l) == rw_all$rare_w_all[i])]}

rare_w_1 <- NA
for (i in 1:10){
  rare_w_1 <- append( rare_w_1, names(  rare_w[ which (names(rare_w) %in% names(which(d$Y_l[which( rownames(d$Y_l) == best_1[i]),]>=1)))]))}
rw_1 <- data.frame(rare_w_1, type = rep(NA, length(rare_w_1)), dim = rep("d1", length(rare_w_1)))
rw_1 <- rw_1[-is.na(rw_1$rare_w_1),]
for ( i in 1: nrow(rw_1)) {
  rw_1$type[i] <- d$type_l[which( colnames(d$Y_l) == rw_1$rare_w_1[i])]}

rare_w_2 <- NA
for (i in 1:10){
  rare_w_2 <- append( rare_w_2, names(rare_w[ which (names(rare_w) %in% names(which(d$Y_l[which( rownames(d$Y_l) == best_2[i]),]>=1)))]))}
rw_2 <- data.frame(rare_w_2, type = rep(NA, length(rare_w_2)), dim = rep("d2", length(rare_w_2)))
rw_2 <- rw_2[-is.na(rw_2$rare_w_2),]
for ( i in 1: nrow(rw_2)) {
  rw_2$type[i] <- d$type_l[which( colnames(d$Y_l) == rw_2$rare_w_2[i])]}

rare_w_3 <- NA
for (i in 1:10){
  rare_w_3 <- append( rare_w_3, names(rare_w[ which (names(rare_w) %in% names(which(d$Y_l[which( rownames(d$Y_l) == best_3[i]),]>=1)))]))}
rw_3 <- data.frame(rare_w_3, type = rep(NA, length(rare_w_3)), dim = rep("d3", length(rare_w_3)))
rw_3 <- rw_3[-is.na(rw_3$rare_w_3),]
for ( i in 1: nrow(rw_3)) {
  rw_3$type[i] <- d$type_l[which( colnames(d$Y_l) == rw_3$rare_w_3[i])]}


rw <- as.data.frame(mapply(c,rw_all, rw_1, rw_2, rw_3))
write.csv(rw, "rare_words.csv", row.names = FALSE)

rw_counts <- as.data.frame(rw %>% group_by(type, dim) %>% count())
ggplot(rw_counts, aes(fill=type, y=n, x=dim)) + 
  geom_bar(position="stack", stat="identity")
rm(rare_w, rare_w_1, rare_w_2, rare_w_3, rare_w_all, 
   rw_1, rw_2, rw_3, rw_all, 
   best_1, best_2, best_3, best_all,
   Ks, Ks_1, Ks_2, Ks_3)

