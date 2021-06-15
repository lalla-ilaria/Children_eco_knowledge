library(scales)
###########
#LOAD DATA#
###########
d <- list.load("2_Data_preparation/processed_data.RData")
post_age <- load("4_Outputs/posteriors/post_age.Rda")
post_act <- load("4_Outputs/posteriors/post_act.Rda")
post_1 <- load("4_Outputs/posteriors/post_1.Rda")
post_2 <- load("4_Outputs/posteriors/post_2.Rda")
post_3 <- load("4_Outputs/posteriors/post_3.Rda")
post_4 <- load("4_Outputs/posteriors/post_4.Rda")

#############################
#GENERAL PLOT SPECIFICATIONS#
#############################
#font
#windowsFonts(A = windowsFont("Garamond"))
#colors
boycol  <- rgb(114/255, 181/255, 40/255) #"navyblue"
girlcol <- rgb(208/255, 29/255, 157/255) #"red3"
col_1 <- "cornflowerblue"
col_2 <- "cornflowerblue"
d$sex_col <- ifelse(d$S == "m", boycol, girlcol)#assign color to each sex
d$color_l <- ifelse( is.na(d$type_l), "slategray1",
             ifelse( d$type_l  == "N", "orange1",
             ifelse( d$type_l  == "S", "deepskyblue3",
             ifelse( d$type_l  == "W", "firebrick2",
             ifelse( d$type_l  == "D", "deeppink3",
             ifelse( d$type_l  == "M", "limegreen", 
             NA))))))

#data
d$A_j <- jitter(d$A)
act_names <- c("household", "seashells", "birds", "game", "agriculture", 
               "livestock", "fishing", "diving", "algae", "cloves")


#######################
#ORDERED CATEGORICAL A#
#######################
#select posterior
post <- post_age

#age-sex specific increase in knowledge
png(file = "4_Outputs/plots/age_sex_knowledge.png", width = 700, height = 500)
  par(mar = c(5,5,2,2) + 0.1)
  year_eff <- apply(post$delta_j, 1, cumsum)
  plot(x =  d$A_j[ d$A_j <= 50 ], 
       y = apply(post$K, 2, mean), 
       xlab = "Age", 
       ylab = "Knowledge", 
       cex.lab=1.5, 
       cex.axis=1.5,
       pch = 19, 
       cex = 1.5, 
       col = alpha( d$sex_col, 0.6 ) ,
       family = "A")
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i] + post$bA[i,1,] * year_eff[,i], 
          type = "l", 
          col = col.alpha( boycol, alpha = 0.1))
  }
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i] + post$bA[i,2,] * year_eff[,i], 
          type = "l", 
          col = col.alpha( girlcol, alpha = 0.1))
  }
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA) + mean(post$bA[,1,]) * apply(year_eff, 1, mean), 
         type = "l", 
         col = col.alpha( boycol, alpha = 0.7))
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA) + mean(post$bA[,2,]) * apply(year_eff, 1, mean), 
         type = "l", 
         col = col.alpha( girlcol, alpha = 0.7))
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



############
#ACTIVITIES#
############
post <- post_act

#activities effect
png(file = "4_Outputs/plots/activities_only.png", width = 700, height = 500)
#age-sex specific knowledge with activities
year_eff <- apply(post$delta_j, 1, cumsum)
par(mar = c(5,5,2,2) + 0.1)
plot(d$A_j[ d$A_j <= 50 ], apply(post$K, 2, mean), 
     xlab = "Age", 
     ylab = "Knowledge", 
     cex.lab=1.5, 
     cex.axis=1.5, 
     pch = 19, 
     cex = 1.5, 
     col = alpha( d$sex_col, 0.6 ) ,
     family = "A" )
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
lines( x = 1:nrow(year_eff),  
       y = mean(post$mA) + mean(post$bA[,1,]) * apply(year_eff, 1, mean), 
       type = "l", 
       col = col.alpha( boycol, alpha = 0.7))
lines( x = 1:nrow(year_eff),  
       y = mean(post$mA) + mean(post$bA[,2,]) * apply(year_eff, 1, mean), 
       type = "l", 
       col = col.alpha( girlcol, alpha = 0.7))

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

png(file = "4_Outputs/plots/activities_effect.png", width = 700, height = 500)
ac <- data.frame("mu" = apply(post$aAM, 2, mean), 
                 "lim5.5" = apply(post$aAM, 2, PI)[1,] , 
                 "lim94.5" = apply(post$aAM, 2, PI)[2,] , 
                 "names" = act_names,
                 "col_0" = ifelse(ac$lim5.5>=0 |ac$lim94.5<=0, col_1, col_2))
ac <- ac[order(ac$mu),]
par(mar = c(5,7,2,2) + 0.1)
plot(x = ac$mu, 
     y = 1:10, 
     xlim = c(-1.6, 1.5),
     pch = 19, 
     mgp=c(2,1,0),
     xlab = "Effect of activities", 
     ylab = "", 
     yaxt='n',
     family = "A",
     col = ac$col_0)
axis(2, ac$names, at = c(1:10), las = 1, family = "A")
abline(v = 0, col = "gray")
for (i in 1:10) lines(c(ac$lim5.5[i],ac$lim94.5[i]) , rep(i, each = 2), col = ac$col_0[i], lwd = 1.5)
dev.off()

#contrasts between sexes    
par(mar = c(5,5,2,2) + 0.1)
plot(NULL, 
     xlim = c(-2, 4), 
     ylim = c(0.5,2.5), 
     yaxt='n',
     xlab = "Age effect for boys - age effect for girls", 
     ylab = "",
     cex.lab=1.5, 
     cex.axis=1.5, 
     family = "A") 
abline(v = 0, col = "grey")
points(mean(post_age$bA[,1,] - post_age$bA[,2,]), 2,
       pch = 19, col = col_1)
points(mean(post_act$bA[,1,] - post_act$bA[,2,]), 1,
       pch = 19, col = col_2)
lines(PI(post_age$bA[,1,] - post_age$bA[,2,]), c(2,2), col = col_1, lwd = 1.5)
lines(PI(post_act$bA[,1,] - post_act$bA[,2,]), c(1,1), col = col_2, lwd = 1.5)
title( "d - Contrasts between sex specific parameters", adj = 0, family = "A")
axis(2, c("Model 2", "Model 1"), at = c(1:2), cex.axis = 1.2, las = 1, family = "A")

##############
# QUADRIPTYCH#
##############
png(file = "4_Outputs/plots/exaptych.png", width = 900, height = 1000)
par( mfcol = c( 3, 2))
#age,sex,knowledge
#!#! changed stuff about title - make sure title is in right position
post <- post_age
    year_eff <- apply(post$delta_j, 1, cumsum)
    par(mar = c(5,5,2,2) + 0.1)
    plot(x = d$A_j[ d$A_j <= 50 ], 
         y = apply(post$K, 2, mean), 
         xlab = "Age", 
         ylab = "Knowledge", 
         pch = 19, 
         cex = 1.5,
         cex.lab = 1.8,
         cex.axis = 1.8,
         col = alpha( d$sex_col, 0.6 ) ,
         family = "A"  )
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post$mA[i] + post$bA[i,1,] * year_eff[,i], 
            type = "l", 
            col = col.alpha( boycol, alpha = 0.1))}
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post$mA[i] + post$bA[i,2,] * year_eff[,i], 
            type = "l", 
            col = col.alpha( girlcol, alpha = 0.1))}
    lines( x = 1:nrow(year_eff),  
           y = mean(post$mA) + mean(post$bA[,1,]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( boycol, alpha = 0.7))
    lines( x = 1:nrow(year_eff),  
           y = mean(post$mA) + mean(post$bA[,2,]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( girlcol, alpha = 0.7))
    title( "a - Age and sex effect", adj = 0, cex.main = 1.8, family = "A")
    legend("bottomright", 
           legend = c("Boys", "Girls"), 
           col = c(boycol, girlcol), 
           pch = 19, 
           bty = "n", 
           cex = 1.5, 
           text.col = "black", 
           horiz = F , 
           inset = c(0.01, 0.01))
#activities effect 
    post <- post_act
    par(mar = c(5,7,2,2) + 0.1)
    # ac <- data.frame("mu" = apply(post$aAM, 2, mean), 
    #                  "lim5.5" = apply(post$aAM, 2, PI)[1,] , 
    #                  "lim94.5" = apply(post$aAM, 2, PI)[2,] , 
    #                  "names" = act_names)
    # ac$col_0 = ifelse(ac$lim5.5>=0 |ac$lim94.5<=0, col_1, col_2)
    # ac <- ac[order(ac$mu),]
    # plot(x = ac$mu, 
    #      y = 1:10, 
    #      xlim = c(-1.6, 1.5),
    #      cex.lab=1.8, 
    #      cex.axis=1.8, 
    #      pch = 19, 
    #      xlab = "Effect of activities", 
    #      ylab = "", 
    #      yaxt='n',
    #      family = "A",
    #      col = ac$col_0)
    # axis(2, ac$names, at = c(1:10), las = 1, cex.axis = 1.2, family = "A")
    # abline(v = 0, col = "gray")
    # for (i in 1:10) lines(c(ac$lim5.5[i],ac$lim94.5[i]) , rep(i, each = 2), col = ac$col_0[i], lwd = 1.5)

    act <- split(post$aAM[,,1], rep(1:ncol(post$aAM[,,1]), each = nrow(post$aAM[,,1])))
    names(act) <- act_names
    act <- act[order(sapply(act, mean))]
    ridgeplot( act,
               step = 1.1,
               xlab = "Effect of activities", 
               cex.lab=1.8, 
               cex.axis=1.8, 
               col = "cornflowerblue",
               fill = col.alpha("cornflowerblue", 0.2),
               family = "A")
    abline(v = 0, col = col.alpha("grey", 0.2))

    title( "b - Activity effects", adj = 0, cex.main = 1.8, family = "A")
    
#age sex effect with activities  
    year_eff <- apply(post$delta_j, 1, cumsum)
    par(mar = c(5,5,2,2) + 0.1)
    plot(d$A_j[ d$A_j <= 50 ], apply(post$K, 2, mean), 
         xlab = "Age", 
         ylab = "Knowledge", 
         cex.lab=1.8, 
         cex.axis=1.8, 
         col = "white" ,
         family = "A" )
    for (i in 1:150) {
      lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,1,] * year_eff[,i], 
            type = "l", 
            col = col.alpha( boycol, alpha = 0.1))}
    for (i in 1:150) {
      lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,2,] * year_eff[,i], 
            type = "l", 
            col = col.alpha( girlcol, alpha = 0.1))}
    lines( x = 1:nrow(year_eff),  
           y = mean(post$mA) + mean(post$bA[,1,]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( boycol, alpha = 0.7))
    lines( x = 1:nrow(year_eff),  
           y = mean(post$mA) + mean(post$bA[,2,]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( girlcol, alpha = 0.7))
    title( "c - Age and sex effect, controlling for activities", adj = 0, cex.main = 1.8, family = "A")
    legend("bottomright", 
           legend = c("Boys", "Girls"), 
           col = c(boycol, girlcol), 
           pch = 19, 
           bty = "n", 
           cex = 1.5, 
           text.col = "black", 
           horiz = F , 
           inset = c(0.01, 0.01))
    
#dimensions    
    post_d <- post_3
    year_eff <- apply(post_d$delta_j[,,1], 1, cumsum)
    plot(x = d$A[ d$A <= 50 ], 
         y = apply(post_d$K[,,1], 2, mean), 
         xlab = "Age", 
         ylab = "Knowledge",  
         cex.lab=1.8, 
         cex.axis=1.8,
         pch = 19, 
         cex = 1.5, 
         col = alpha( d$sex_col, 0.6 ) ,
         family = "A" )
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,1] + post_d$bA[i,1,1] * year_eff[,i], 
            type = "l", 
            col = col.alpha( boycol, alpha = 0.1))}
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,1] + post_d$bA[i,2,1] * year_eff[,i], 
            type = "l", 
            col = col.alpha( girlcol, alpha = 0.1))}
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,1]) + mean(post_d$bA[,1,1]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( boycol, alpha = 0.7))
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,1]) + mean(post_d$bA[,2,1]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( girlcol, alpha = 0.7))
    title( "d - Age and sex effect dimension 1", adj = 0, cex.main = 1.8, family = "A")
    legend("bottomright", 
           legend = c("Boys", "Girls"), 
           col = c(boycol, girlcol), 
           pch = 19, 
           bty = "n", 
           cex = 1.5, 
           text.col = "black", 
           horiz = F , 
           inset = c(0.01, 0.01))
    
    year_eff <- apply(post_d$delta_j[,,2], 1, cumsum)
    plot(x = d$A[ d$A <= 50 ], 
         y = apply(post_d$K[,,2], 2, mean), 
         xlab = "Age", 
         ylab = "Knowledge",  
         cex.lab=1.8, 
         cex.axis=1.8,  
         pch = 19, 
         cex = 1.5, 
         col = alpha( d$sex_col, 0.6 ) ,
         family = "A")
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,2] + post_d$bA[i,1,2] * year_eff[,i], 
            type = "l", 
            col = col.alpha( boycol, alpha = 0.1))}
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,2] + post_d$bA[i,2,2] * year_eff[,i], 
            type = "l", 
            col = col.alpha( girlcol, alpha = 0.1))}
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,2]) + mean(post_d$bA[,1,2]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( boycol, alpha = 0.7))
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,2]) + mean(post_d$bA[,2,2]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( girlcol, alpha = 0.7))
    title( "e - Age and sex effect dimension 2", adj = 0, cex.main = 1.8, family = "A")
    legend("bottomright", 
           legend = c("Boys", "Girls"), 
           col = c(boycol, girlcol), 
           pch = 19, 
           bty = "n", 
           cex = 1.5, 
           text.col = "black", 
           horiz = F , 
           inset = c(0.01, 0.01))
    
    year_eff <- apply(post_d$delta_j[,,3], 1, cumsum)
    plot(x = d$A[ d$A <= 50 ], 
         y = apply(post_d$K[,,3], 2, mean), 
         xlab = "Age", 
         ylab = "Knowledge",  
         cex.lab=1.8, 
         cex.axis=1.8, 
         cex.main=1.5, 
         pch = 19, 
         cex = 1.5, 
         col = alpha( d$sex_col, 0.6 ) ,
         family = "A" )
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,3] + post_d$bA[i,1,3] * year_eff[,i], 
            type = "l", 
            col = col.alpha( boycol, alpha = 0.1))}
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,3] + post_d$bA[i,2,3] * year_eff[,i], 
            type = "l", 
            col = col.alpha( girlcol, alpha = 0.1))}
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,3]) + mean(post_d$bA[,1,3]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( boycol, alpha = 0.7))
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,3]) + mean(post_d$bA[,2,3]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( girlcol, alpha = 0.7))
    title( "f - Age and sex effect dimension 3", adj = 0, cex.main = 1.8, family = "A")
    legend("bottomright", 
           legend = c("Boys", "Girls"), 
           col = c(boycol, girlcol), 
           pch = 19, 
           bty = "n", 
           cex = 1.5, 
           text.col = "black", 
           horiz = F , 
           inset = c(0.01, 0.01))
    dev.off()

####################
#DIMENSION ANALYSIS#
####################
#Dimension specific age-sex effects
png(file = "4_Outputs/plots/dimensions.png", width = 500, height = 1000)
post_d <- post_3
par( mfrow = c( 3, 1))
year_eff <- apply(post_d$delta_j[,,1], 1, cumsum)
    plot(x = d$A[ d$A <= 50 ], 
         y = apply(post_d$K[,,1], 2, mean), 
         xlab = "Age", 
         ylab = "Knowledge", 
         main = "Dimension 1", 
         cex.lab=1.8, 
         cex.axis=1.8,  
         cex.main=1.8,
         pch = 19, 
         cex = 1.5, 
         col = alpha( d$sex_col, 0.6 ) ,
         family = "A" )
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,1] + post_d$bA[i,1,1] * year_eff[,i], 
            type = "l", 
            col = col.alpha( boycol, alpha = 0.1))}
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,1] + post_d$bA[i,2,1] * year_eff[,i], 
            type = "l", 
            col = col.alpha( girlcol, alpha = 0.1))}
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,1]) + mean(post_d$bA[,1,1]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( boycol, alpha = 0.7))
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,1]) + mean(post_d$bA[,2,1]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( girlcol, alpha = 0.7))
    
year_eff <- apply(post_d$delta_j[,,2], 1, cumsum)
    plot(x = d$A[ d$A <= 50 ], 
         y = apply(post_d$K[,,2], 2, mean), 
         xlab = "Age", 
         ylab = "Knowledge", 
         main = "Dimension 2", 
         cex.lab=1.8, 
         cex.axis=1.8,  
         cex.main=1.8,
         pch = 19, 
         cex = 1.5, 
         col = alpha( d$sex_col, 0.6 ) ,
         family = "A")
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,2] + post_d$bA[i,1,2] * year_eff[,i], 
            type = "l", 
            col = col.alpha( boycol, alpha = 0.1))}
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,2] + post_d$bA[i,2,2] * year_eff[,i], 
            type = "l", 
            col = col.alpha( girlcol, alpha = 0.1))}
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,2]) + mean(post_d$bA[,1,2]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( boycol, alpha = 0.7))
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,2]) + mean(post_d$bA[,2,2]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( girlcol, alpha = 0.7))
    legend("bottomright", 
           legend = c("Boys", "Girls"), 
           col = c(boycol, girlcol), 
           pch = 19, 
           bty = "n", 
           cex = 1.5, 
           text.col = "black", 
           horiz = F , 
           inset = c(0.1, 0.1))
year_eff <- apply(post_d$delta_j[,,3], 1, cumsum)
    plot(x = d$A[ d$A <= 50 ], 
         y = apply(post_d$K[,,3], 2, mean), 
         xlab = "Age", 
         ylab = "Knowledge", 
         main = "Dimension 3", 
         cex.lab=1.8, 
         cex.axis=1.8, 
         cex.main=1.8, 
         pch = 19, 
         cex = 1.5, 
         col = alpha( d$sex_col, 0.6 ) ,
         family = "A" )
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,3] + post_d$bA[i,1,3] * year_eff[,i], 
            type = "l", 
            col = col.alpha( boycol, alpha = 0.1))}
    for (i in 1:150) {
      lines(x = 1:nrow(year_eff),  
            y = post_d$mA[i,3] + post_d$bA[i,2,3] * year_eff[,i], 
            type = "l", 
            col = col.alpha( girlcol, alpha = 0.1))}
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,3]) + mean(post_d$bA[,1,3]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( boycol, alpha = 0.7))
    lines( x = 1:nrow(year_eff),  
           y = mean(post_d$mA[,3]) + mean(post_d$bA[,2,3]) * apply(year_eff, 1, mean), 
           type = "l", 
           col = col.alpha( girlcol, alpha = 0.7))
    
dev.off()
    
    
    
##########################################
#SUPPLEMENTARY############################
##########################################


##################
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

