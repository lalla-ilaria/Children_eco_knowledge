library(scales)
library(rethinking)
library(rlist)
library(ggfree)
library(tidyverse)
library(gtools)

###########
#LOAD DATA#
###########
d <- list.load("2_Data_preparation/processed_data.RData")

#####################
###LOAD FIT MODELS###
#AND EXTRACT SAMPLES#
#####################
# <- readRDS("4_Outputs/posteriors/.rds")
file_list <- list.files(path="3_Analysis/fit_models/")

for (i in 1:length(file_list)){
  temp_mod <- readRDS( paste("3_Analysis/fit_models/", file_list[[i]], sep = ""))
  assign ( gsub(".rds", "", file_list[i]),   temp_mod)
  assign ( paste ("post", gsub(".rds", "", file_list[i]), sep = "_") , extract.samples(temp_mod))
}

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

#jitter ages
d$A_j <- jitter(d$A)


################
#PLOT FUNCTIONS#
################
########################################
#Age and sex curves predicted by models#
########################################

#age and sex specific estimated knowledge with or without dots
plotagesandknow <- function(d = d, post , dimn, dots = T, ages = d$A_j[ d$A_j <= 50 ], maintitle = "") {
  year_eff <- apply(post$delta_j[,,dimn], 1, cumsum)
  plot(x = ages, 
       y = apply(post$K[,,dimn], 2, mean), 
       xlab = "Age" , 
       ylab = "",
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
  title( main = maintitle, cex.main = 1.8, ylab = "Knowledge", line=1, cex.lab=1.8)
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

##########################################
#############make function to#############
#calculate years gained by counterfactual#
##########################################
diffage <- function ( post , counterfactual_1 = 0, counterfactual_2 ,  
                      year = 20, sex = 2 , dimn = 1) {
  #prepare data
  x <- seq(1,27) #gives sequence of age to fit the function
  if(length(counterfactual_1)<=1) counterfactual_1 <- rep(counterfactual_1, 500)
  if(length(counterfactual_2)<=1) counterfactual_2 <- rep(counterfactual_2, 500)
  year_eff <- apply(post$delta_j[,,dimn], 1, cumsum)
  
  #find mean difference
  #first counterfactual (or baseline)
  #age_val is the value of y (knowledge) at the "years" age in the counterfactual 1 condition
  age_val  <- mean(post$mA[,dimn]) + mean(post$bA[,sex,dimn]) * apply(year_eff, 1, mean)[year] + mean(counterfactual_1)
  #2nd counterfactual
  #spline is the function of knowledge over age in the main counterfactual considered (2)
  spline  <- smooth.spline ( 
    mean( post$mA[,dimn]) + mean(post$bA[,sex,dimn]) * apply(year_eff, 1, mean) + mean(counterfactual_2) ~ x)
  #find x value (age) in the counterfactual 2 condition for which y value (knowledge) is the same as at age 'years' in the counterfactual 1 condition
  if (inherits( try( solve(uniroot(function(x) predict(spline, x, deriv = 0)$y - age_val ,
                                   interval = c(0, 70))$root), silent=TRUE),  "try-error")) {
    curve(predict(spline, x, deriv = 0)$y, 0, 40)
    print("Mean new age greater than 40 - mean not calculated")
  } else { 
    newyear <- uniroot(function(x) predict(spline, x, deriv = 0)$y - age_val ,
                       interval = c(0, 70))$root
  }
  mean_diff_age <- year - newyear

  #calculate distribution of years gained by counterfactual
  diff_age <- vector(mode = "numeric", length = 500)
  #loop to calculate distribution
  for ( i in 1:500){
    #first counterfactual (or baseline)
    age_val  <- post$mA[i,dimn] + post$bA[i,sex,dimn] * year_eff[year,i] + counterfactual_1[i]
    #2nd counterfactual
    spline  <- smooth.spline ( post$mA[i,dimn] + post$bA[i,sex,dimn] * year_eff[,i] + counterfactual_2[i] ~ x,
                               spar = 0.5)
    #find x value (age) in the counterfactual 2 condition for which y value (knowledge) is the same as at age 'years' in the counterfactual 1 condition
    #smoothed lines do not always cross zero (i.e. the age expected for someone who has the same knowledge as counterfactual 1 is greater than 70)
    #in that case the error is suppressed and we get an image of the age curve that did not reach zero
    if (inherits( try( solve(uniroot(function(x) predict(spline, x, deriv = 0)$y - age_val ,
                                     interval = c(0, 70))$root), silent=TRUE),  "try-error")) {
      curve(predict(spline, x, deriv = 0)$y, 0, 30)
      abline ( h = 0)
      title( i)
      newyear <- NA 
    } else { 
      newyear <- uniroot(function(x) predict(spline, x, deriv = 0)$y - age_val ,
                         interval = c(0, 70))$root
    }
    diff_age[i] <- year - newyear
  }#distr
  return(diff_age)
}

##########################
#Plot activities effects##
##########################
#distributuions can be plotted as units of effect or as age differences
#in this case, the value on the x axis indicates how many years an individual would gain by practicing an activity
#i.e. how many years earlier (positive values) or later (negative value) an individual practicing an activity 
#would reach the knowledge an individual not practicing that activity would have at 'years' age

  #NB the calculation relies on the uniroot function, which calculates the unique root of the function. But because 
  #it fits a spline, sometime there isn't an unique root at all, or there are multiple. Error messages and plots appear, 
  #but as long as there aren't many values that cannot be calculated, the results are valid
plotact <- function( post, dimn = 1, unit = "years") {
  act <- list()
  if ( unit == "years" )  { 
    for ( i in 1:d$C) {
      act [[i]] <- diffage( post = post , dimn = dimn, counterfactual_2 = post$aAM[,i,dimn])
      print( paste ( sum(is.na(act[[i]]) ), "new ages greater than 70 in activity" , i) )
      act [[i]] <- act[[i]][!is.na(act[[i]])]}
  } else {act <- split(post$aAM[,,dimn], rep(1:ncol(post$aAM[,,dimn]), each = nrow(post$aAM[,,dimn])))}
  names(act) <- c("household", "seashells", "birds", "game", "agriculture", 
                  "livestock", "fishing", "diving", "algae", "cloves")
  act <- act[order(sapply(act, mean))]
  ridgeplot( act,
             if (unit == "years") { xlim =c(- 20, 10) 
             } else {xlim =   c(-2,2)},
             step = ifelse(unit == "years" , 0.11 , 1.3 ),
             xlab = ifelse(unit == "years" , "Years gained practicing activities" , "Effect of activities"), 
             cex.lab=1, 
             cex.axis=1, 
             col = "cornflowerblue",
             #family = "A",
             fill = col.alpha("cornflowerblue", 0.2))
  abline(v = 0, col = col.alpha("grey", 0.2))
  return(act)
}


##########################
#Plot IRT curves##########
##########################

plotirtcurves <- function(post , qn = 1 , dimn = 1 , maintitle = "", 
                          col_lines = "lightblue", col_dots = "cornflowerblue"){
  if(qn == 1)       {n_lines <- length (post$a_l[1,,dimn])
  }else if (qn == 2){n_lines <- length (post$a_q[1,,dimn])
  }else if (qn == 3){n_lines <- length (post$a_r[1,,dimn])}
  if(length(col_lines)<=1) col_lines <- rep(col_lines, n_lines)
  if(qn == 1){
    a <- apply(post$a_l[,,dimn], 2, mean)
    b <- apply(post$b_l[,,dimn], 2, mean)
  } else if (qn == 2) {
    a <- apply(post$a_q[,,dimn], 2, mean)
    b <- apply(post$b_q[,,dimn], 2, mean)
    c <- apply(post$c_q[,], 2, mean)
  }
  else if (qn == 3) {
    a <- apply(post$a_r[,,dimn], 2, mean)
    b <- apply(post$b_r[,,dimn], 2, mean)
  }
  
  curve(inv_logit(a[1] * ( x - b[1])), 
        xlim = c(-11, 4), 
        ylim = c(0, 1), 
        xlab = "knowledge dimension", 
        ylab = "p correct answer", 
        cex.lab=1.5, 
        cex.axis=1.5,
        col = "white")
  title(main = maintitle, cex.main=1.5)
  for(i in 1: n_lines){
    if (qn == 2) {
      curve(inv_logit(log( exp( a[i] * ( x - b[i])) + c[i] ) - log( 1 - c[i] )), 
            col = alpha(col_lines[i], 0.4), 
            add = TRUE)}else{
      curve(inv_logit(a[i] * ( x - b[i])), 
          col = alpha(col_lines[i], 0.4), 
          add = TRUE)}
  }
  points(apply(post$K, 2, mean), rep(0.5, 93),
         pch = 19, 
         cex = 1.5, 
         col = alpha(col_dots, 0.4))  
}



#######################
#year specific effects#
#######################

plotdeltaffect <- function (deltaj , x_lab = "Age specific increase", y_lab = "Ages") {
  plot(apply(deltaj, 2, mean), 1:ncol(deltaj), 
     xlab = x_lab, ylab = y_lab,
     pch = 19, col = col_1)
  for (i in 1:ncol(deltaj)) lines(apply(deltaj, 2, PI)[,i], rep(i, each = 2), col = col_1, lwd = 1.5)
}

plot(apply(post_bor_1$delta_jb, 2, mean), 1:ncol(post_bor_1$delta_jb), 
     #xlab = x_lab, ylab = y_lab,
     pch = 19, col = col_1)
for (i in 1:ncol(post_bor_1$delta_jb)) lines(apply(post_bor_1$delta_jb, 2, PI)[,i], rep(i, each = 2), col = col_1, lwd = 1.5)


###############################
#effect of not going to school#
###############################
#bSY describe difference in knowledge of an individual who did not go to school, vs a baseline of an individual who did
#the plot shows the difference in years-knowledge of an individual who did not study vs someone who did
#intermediate levels of schooling are described by portions of bSY defined by a delta_js variable
plotschooleffect <- function( post, dimn = 1) {
  act <- list()
  act [[1]] <- diffage( post = post , dimn = dimn, counterfactual_2 = post$bSY[,1,1], sex = 1)
  act [[2]] <- diffage( post = post , dimn = dimn, counterfactual_2 = post$bSY[,2,1], sex = 2)
  for ( i in 1:2) {
    act [[i]] <- act[[i]][!is.na(act[[i]])]}
  names(act) <- c("School - boys", "School - girls")
  ridgeplot( act,
             step = 0.1,
             xlab = paste("Years gained skipping school"), 
             cex.lab=1, 
             cex.axis=1, 
             col = "cornflowerblue",
             #family = "A",
             fill = col.alpha("cornflowerblue", 0.2))
  abline(v = 0, col = col.alpha("grey", 0.2))
}

#####################
#presence of parents#
#####################

plotparentseffect <- function( post, dimn ) {
  act <- list()
  act [[1]] <- diffage( post = post , dimn = dimn, counterfactual_2 = post$bMP[,2,1], sex = 2)
  act [[2]] <- diffage( post = post , dimn = dimn, counterfactual_2 = post$bMP[,1,1], sex = 1)
  act [[3]] <- diffage( post = post , dimn = dimn, counterfactual_2 = post$bFP[,2,1], sex = 2)
  act [[4]] <- diffage( post = post , dimn = dimn, counterfactual_2 = post$bFP[,1,1], sex = 1)
  for ( i in 1:4) {
    act [[i]] <- act[[i]][!is.na(act[[i]])]}
  #act <- split(post$aAM[,,dimn], rep(1:ncol(post$aAM[,,dimn]), each = nrow(post$aAM[,,dimn])))
  names(act) <- c("Mother - girls", "Mother - boys", 
                  "Father - girls", "Father - boys")
  ridgeplot( act,
             step = 0.1,
             xlab = paste("Years gained with presence of parent"), 
             xlim = c(-30,20),
             cex.lab=1, 
             cex.axis=1, 
             col = "cornflowerblue",
             #family = "A",
             fill = col.alpha("cornflowerblue", 0.2))
  abline(v = 0, col = col.alpha("grey", 0.2))
  return(act)
}

########################
#plot prior simulations#
########################

plotpriors <- function(post , dimn = 1) {
  mA <- -abs(rnorm (150, 5, 3 ) )
  bA <-  abs(rnorm(150, 3 , 2 ) )
  delta <- rdirichlet(150, rep(0.5, 27) )
  year_eff_sim <- apply(delta, 1, cumsum)
  year_eff <- apply(post$delta_j[,,dimn], 1, cumsum)
  plot(x = 1:26, 
       y = seq(-10,5, length.out = 26),
       xlim = c(1.8,26),
       xlab = "Ages" , 
       ylab = "",
       cex.lab=1.8 , 
       cex.axis=1.8 ,
       pch = 19 , 
       cex =  0 , 
       #family = "A",
       col =  "white")
  for (i in 1:150) {
    lines(x = 1:27,  
          y = mA[i] + bA[i] * year_eff_sim[,i], 
          type = "l", 
          col = col.alpha( "lightblue", alpha = 0.6))}
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,1,dimn]) * apply(year_eff, 1, mean), 
         type = "l", 
         col = col.alpha( boycol, alpha = 0.7))
  lines( x = 1:nrow(year_eff),  
         y = mean(post$mA[,dimn]) + mean(post$bA[,2,dimn]) * apply(year_eff, 1, mean), 
         type = "l", 
         col = col.alpha( girlcol, alpha = 0.7))
}

