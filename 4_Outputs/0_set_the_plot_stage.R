library(scales)
library(rethinking)
library(rlist)
library(ggfree)

###########
#LOAD DATA#
###########
d <- list.load("2_Data_preparation/processed_data.RData")

#####################
#####LOAD MODELS#####
#AND EXTRACT SAMPLES#
#####################
# <- readRDS("4_Outputs/posteriors/.rds")
post_age_1 <- extract.samples(age_1)
post_age_2 <- extract.samples(age_2)
post_age_3 <- extract.samples(age_3)
post_age_4 <- extract.samples(age_4)
post_age_5 <- extract.samples(age_5)

post_act_1 <- extract.samples(act_1)
post_act_3 <- extract.samples(act_3)

post_age_noq_1 <- extract.samples(age__noq1)
post_age_noq_3 <- extract.samples(age__noq3)

post_actssp_1 <- extract.samples(act_actssp_1)
post_actssp_3 <- extract.samples(act_actssp_3)

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


################
#PLOT FUNCTIONS#
################

#age and sex specific estimated knowledge with or without dots
plotagesandknow <- function(d = d, post , dimn, dots = T) {
  year_eff <- apply(post$delta_j[,,dimn], 1, cumsum)
  plot(x = d$A_j[ d$A_j <= 50 ], 
       y = apply(post$K[,,dimn], 2, mean), 
       xlab = "Age", 
       ylab = "Knowledge",  
       cex.lab=1.8, 
       cex.axis=1.8,
       pch = 19, 
       cex = ifelse( dots == T, 1.5, 0), 
       family = "A",
       col =  alpha( d$sex_col, 0.6 )  )
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i,dimn] + post$bA[i,1,dimn] * year_eff[,i], 
          type = "l", 
          col = col.alpha( boycol, alpha = 0.1))}
  for (i in 1:150) {
    lines(x = 1:nrow(year_eff),  
          y = post$mA[i,dimn] + post$bA[i,2,dimn] * year_eff[,i], 
          type = "l", 
          col = col.alpha( girlcol, alpha = 0.1))}
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,1,dimn]) * apply(year_eff, 1, mean), 
         type = "l", 
         col = col.alpha( boycol, alpha = 0.7))
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,2,dimn]) * apply(year_eff, 1, mean), 
         type = "l", 
         col = col.alpha( girlcol, alpha = 0.7))
  title( "d - Age and sex effect dimension 1", adj = 0, cex.main = 1.8)
  legend("bottomright", 
         legend = c("Boys", "Girls"), 
         col = c(boycol, girlcol), 
         pch = 19, 
         bty = "n", 
         cex = 1.5, 
         text.col = "black", 
         horiz = F , 
         inset = c(0.01, 0.01))
}

plotact <- function( post, dimn ) {
  act <- split(post$aAM[,,dimn], rep(1:ncol(post$aAM[,,dimn]), each = nrow(post$aAM[,,dimn])))
  names(act) <- c("household", "seashells", "birds", "game", "agriculture", 
                  "livestock", "fishing", "diving", "algae", "cloves")
  act <- act[order(sapply(act, mean))]
  ridgeplot( act,
             step = 1.1,
             xlab = "Effect of activities1", 
             cex.lab=1.8, 
             cex.axis=1.8, 
             col = "cornflowerblue",
             #family = "A",
             fill = col.alpha("cornflowerblue", 0.2))
  abline(v = 0, col = col.alpha("grey", 0.2))
}

plotact_bysex <- function( post, dimn ) {
  act_m <- split(post$aAM[,1,,dimn], rep(1:ncol(post$aAM[,1,,dimn]), each = nrow(post$aAM[,1,,dimn])))
  act_f <- split(post$aAM[,2,,dimn], rep(1:ncol(post$aAM[,2,,dimn]), each = nrow(post$aAM[,2,,dimn])))
  names(act_m) <- c("household", "seashells", "birds", "game", "agriculture", 
                  "livestock", "fishing", "diving", "algae", "cloves")
  names(act_f) <- c("household", "seashells", "birds", "game", "agriculture", 
                    "livestock", "fishing", "diving", "algae", "cloves")
  #act <- act[order(sapply(act, mean))]
  par(mfrow = c(1,2))
  ridgeplot( act_m,
             step = 1.1,
             xlab = "Effect of activities1", 
             cex.lab=1.8, 
             cex.axis=1.8, 
             col = boycol,
             #family = "A",
             fill = col.alpha(boycol, 0.2))
  abline(v = 0, col = col.alpha("grey", 0.2))
  ridgeplot( act_f,
             step = 1.1,
             xlab = "Effect of activities1", 
             cex.lab=1.8, 
             cex.axis=1.8, 
             col = girlcol,
             #family = "A",
             fill = col.alpha(girlcol, 0.2))
  abline(v = 0, col = col.alpha("grey", 0.2))
}
