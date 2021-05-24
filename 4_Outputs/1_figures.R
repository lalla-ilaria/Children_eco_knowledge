library(scales)
########
#COLORS#
########
#colors
boycol  <- "navyblue"
girlcol <- "red3"
d$sex_col <- ifelse(d$S == "m", boycol, girlcol)#assign color to each sex


#######################
#ORDERED CATEGORICAL A#
#######################
#extract samples
post_age <- extract.samples(m_age)
post <- post_age

#INDIVIDUALS
############
#age-sex specific increase in knowledge
png(file = "4_Outputs/plots/age_sex_knowledge.png", width = 800, height = 500)
#mA + aK[i,d] + bA[S[i], d] * sum (delta_j[ 1 : A[i] ] ) ; 
year_eff <- apply(post$delta_j, 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post$K, 2, mean), 
     xlab = "Age", 
     ylab = "Knowledge", 
     cex.lab=1.7, 
     cex.axis=1.5,
     pch = 19, 
     cex = 1.5, 
     col = alpha( d$sex_col, 0.6 ) )
for (i in 1:150) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,1,] * year_eff[,i], 
        type = "l", 
        col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:150) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,2,] * year_eff[,i], 
        type = "l", 
        col = col.alpha( girlcol, alpha = 0.1))
}
legend("bottomright", 
       legend = c("Boys", "Girls"), 
       col = c(boycol, girlcol), 
       pch = 19, 
       bty = "n", 
       cex = 1.5, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dev.off()


#######
#ITEMS
#image recognition
a_rs <- apply(post$a_r, 2, mean)
b_rs <- apply(post$b_r, 2, mean)

png(file = "4_Outputs/plots/item_parameter_recognition.png", width = 600, height = 400)
curve(inv_logit(a_rs[1] * ( x - b_rs[1])), 
      xlim = c(-11, 4), 
      ylim = c(0, 1), 
      xlab = "knowledge in image recognition", 
      ylab = "p correct answer", 
      cex.lab=1.5, 
      cex.axis=1.5,
      col = col.alpha("lightblue", 0.6))
for(i in 1: length(a_rs)){
  curve(inv_logit(a_rs[i] * ( x - b_rs[i])), 
        col = col.alpha("lightblue", 0.7), 
        add = TRUE)
}
points(apply(post$K, 2, mean), rep(0.5, 93),
       pch = 19, 
       cex = 1.5, 
       col = col.alpha("cornflowerblue", 0.4))  
dev.off()



############
#ACTIVITIES#
############
post_act <- extract.samples(m_act)
post <- post_act
act_names <- c("household", "seashells", "birds", "game", "agriculture", 
               "livestock", "fishing", "spearfishing", "algae", "cloves")

#activities effect
png(file = "4_Outputs/plots/activities_only.png", width = 800, height = 500)
#par( mfrow = c( 1, 2))
#age-sex specific knowledge with activities
plot(d$A[ d$A <= 50 ], apply(post$K, 2, mean), 
     xlab = "Age", 
     ylab = "Knowledge", 
     cex.lab=1.5, 
     cex.axis=1.5, 
     pch = 19, 
     cex = 1.5, 
     col = alpha( d$sex_col, 0.6 ) )
for (i in 1:150) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,1,] * year_eff[,i], 
        type = "l", 
        col = col.alpha( boycol, alpha = 0.1))
}
for (i in 1:150) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,2,] * year_eff[,i], 
        type = "l", 
        col = col.alpha( girlcol, alpha = 0.1))
}
legend("bottomright", 
       legend = c("Boys", "Girls"), 
       col = c(boycol, girlcol), 
       pch = 19, 
       bty = "n", 
       cex = 1.5, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

# plot(apply(post$aAM, 2, mean), 1:10, xlim = c(-1.6, 1.5),
#      pch = 19, mgp=c(1.4,0.5,0),
#      xlab = "Effect of activities", ylab = "", yaxt='n')
# axis(2, act_names, at = c(1:10), las = 1)
# abline(v = 0, col = "gray")
# for (i in 1:10) lines(apply(post$aAM, 2, PI)[,i], rep(i, each = 2))

dev.off()



##################
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

