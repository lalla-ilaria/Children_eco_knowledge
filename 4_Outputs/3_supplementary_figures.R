#Set the stage for plotting figures
if( !exists( "plotagesandknow", mode = "function")) source( "4_Outputs/0_set_the_plot_stage.R" )

###########################
#Sampling bias#############
###########################

#with subset of people living close to research station
png(file = "4_Outputs/plots/supplementary_figures/ngezi_subset.png", width = 600, height = 400)
plotagesandknow(d=d, post = `post_nge`, dimn =1, ages = d$A_j[d$HH<=100&d$A<=50])
dev.off()

#including distance as a predictor
png(file = "4_Outputs/plots/supplementary_figures/controlled_distance.png", width = 600, height = 400)
plotagesandknow(d=d, post = `post_dis`, dimn =1, ages = d$A_j[!is.na(d$HH_dist<=100)&d$A<=50])
dev.off()

#############################
#IRT curves by question type#
#############################

png(file = "4_Outputs/plots/supplementary_figures/IRTcurves.png", width = 700, height = 300)
par(mfrow = c(1,3),oma = c(0,1,0,0))
plotirtcurves(post = post_age_1, qn = 1, maintitle = "a - Freelist")
plotirtcurves(post = post_age_1, qn = 2, maintitle = "b - Questionnaire")
plotirtcurves(post = post_age_1, qn = 3, maintitle = "c - Image recognition")
dev.off()

png(file = "4_Outputs/plots/supplementary_figures/IRTdimension.png", width = 700, height = 300)
par(mfrow = c(1,3),oma = c(0,1,0,0))
plotirtcurves(post = post_age_3, qn = 1, dimn = 2, col_lines = d$color_l)
plotirtcurves(post = post_age_3, qn = 1, dimn = 3, col_lines = d$color_l)
plotirtcurves(post = post_age_3, qn = 1, dimn = 1, col_lines = d$color_l)
dev.off()

##########################
#Year specific effect#####
##########################
#explore age effects
#plot (precis(m_age, 3, pars = "delta_j"))
png(file = "4_Outputs/plots/supplementary_figures/year_effects.png", width = 400, height = 400)
plotdeltaffect (deltaj = post_age_1$delta_j)
dev.off()

#####################################
#IRT curves varying difficulty prior#
#######and prior simulation##########
#####################################

png(file = "4_Outputs/plots/supplementary_figures/IRTdiffprior.png", width = 700, height = 300)
par(mfrow = c(1,3),oma = c(0,1,0,0))
plotirtcurves(post = post_age_1, qn = 1, maintitle = "a")
plotirtcurves(post = post_age_bs_1, qn = 1, maintitle = "b")
plotirtcurves(post = post_age_bs_3, qn = 1, maintitle = "c")
dev.off()


png(file = "4_Outputs/plots/supplementary_figures/sim_priors_foragesex.png", width = 600, height = 400)
par(oma = c(0,1,0,0))
plotpriors(post = post_age_1)
dev.off()

png(file = "4_Outputs/plots/supplementary_figures/sim_priors_IRTqns.png", width = 600, height = 400)
curve( inv_logit(rlnorm(1, 0, 1) * ( x - rnorm(1, 0, 2) ) ), 
       xlim = c(-8, 8) , ylim = c(0, 1) ,
       col = col.alpha( "lightblue", alpha = 0.6),
       xlab = "Knowledge dimension", ylab = "")
for(i in 1:150) {
  curve( inv_logit(rlnorm(1, 0, 1) * ( x - rnorm(1, 0, 2) ) ), 
         col = col.alpha( "lightblue", alpha = 0.6),
         add = TRUE)
}
dev.off()



###########################
#ACTIVITIES################
###########################

#activities effect
#age-sex specific knowledge with activities effect removed
png(file = "4_Outputs/plots/supplementary_figures/zero_activities.png", width = 600, height = 400)
par(oma = c(0,1,0,0))
plotagesandknow(d=d, post = `post_act_1`, dimn = 1, dots = F)
dev.off()

png(file = "4_Outputs/plots/supplementary_figures/zero_activities_dimensions.png", width = 600, height = 400)
par(oma = c(0,1,0,0), mfrow = c(1,3))
plotagesandknow(d=d, post = `post_act_3`, dimn = 1, dots = F)
plotagesandknow(d=d, post = `post_act_3`, dimn = 2, dots = F)
plotagesandknow(d=d, post = `post_act_3`, dimn = 3, dots = F)
dev.off()

#Activities effects 

png(file = "4_Outputs/plots/supplementary_figures/activities_effect.png", width = 400, height = 300)
par(mar = c(5,5,2,2) + 0.1, mfrow = c(1,1))
plotact(post = post_act_1, unit = "effect")
dev.off()

############################
#Family stuff###############
############################

#Presence of parents
#seems like there's some difference, boys have benefit from having father close by, girls don't
png(file = "4_Outputs/plots/supplementary_figures/parents_years.png", width = 400, height = 300)
par(mar = c(5,7,2,2) + 0.1, mfrow = c(1,1))
pardiff <- plotparentseffect(post = post_epp_1)
dev.off()

#calculate tail of the distributions below -30 years
for (i in 1:4){
  print ( paste ( sum (pardiff[[i]] <= -20) / 500, "percentile of the distribution for", names(pardiff)[i] , "dyad below -30 years"))
}

#household effect
#seems to be no effect of household
png(file = "4_Outputs/plots/supplementary_figures/hh_random_effects.png", width = 400, height = 400)
plot(1:36, apply(post_act_1$aH, 2, mean),
     xlab = "Household", ylab = "Household random effect",
     pch = 19, col = col_2)
for (i in 1 : 36) lines( c(i,i), apply(post_act_1$aH, 2, PI)[,i], col = col_2, lwd = 1.5)
dev.off()

#checks correspondent hh_id to household number in model results
unique(as.data.frame(d[c(3,length(d))]))

#plot effect of each birth order position
#seems to be no effect of birth order
png(file = "4_Outputs/plots/supplementary_figures/birthorder_effects.png", width = 400, height = 400)
plotdeltaffect(deltaj = post_bor_1$delta_jb, x_lab = "Effect of being in each sibship position", y_lab = "Sibship order")
dev.off()

############################
#School effect##############
############################

#there seem to be no effect of school
plotdeltaffect(deltaj = post_sch_1$delta_js, x_lab = "Effect of school years lost", y_lab = "Years of school lost")

png(file = "4_Outputs/plots/supplementary_figures/school_years.png", width = 400, height = 300)
par(mar = c(5,7,2,2) + 0.1, mfrow = c(1,1))
plotschooleffect(post = post_sch_1)
dev.off()

########################
#DIMENSIONS#############
########################

#plot waics by data type
png(file = "4_Outputs/plots/supplementary_figures/waics.png", width = 700, height = 300)
par(mfrow = c(1,3))
plot(waics_age_l, main = "WAIC values for freelist")
plot(waics_age_q, main = "WAIC values for questionnaire")
plot(waics_age_r, main = "WAIC values for image recognition")
dev.off()

#2 dimensions
png(file = "4_Outputs/plots/supplementary_figures/2_dim.png", width = 600, height = 380)
par(mfrow = c(1,2),oma = c(0,1,0,0))
plotagesandknow(d=d, post = `post_age_2`, dimn=2)
plotagesandknow(d=d, post = `post_age_2`, dimn=1)
dev.off()

#4 dimensions
png(file = "4_Outputs/plots/supplementary_figures/4_dim.png", width = 600, height = 600)
par(mfrow = c(2,2),oma = c(0,1,0,0))
plotagesandknow(d=d, post = `post_age_4`, dimn=2)
plotagesandknow(d=d, post = `post_age_4`, dimn=4)
plotagesandknow(d=d, post = `post_age_4`, dimn=3)
plotagesandknow(d=d, post = `post_age_4`, dimn=1)
dev.off()

#5 dimensions
png(file = "4_Outputs/plots/supplementary_figures/5_dim.png", width = 800, height = 600)
par(mfrow = c(2,3),oma = c(0,1,0,0))
plotagesandknow(d=d, post = `post_age_5`, dimn=5)
plotagesandknow(d=d, post = `post_age_5`, dimn=1)
plotagesandknow(d=d, post = `post_age_5`, dimn=3)
plotagesandknow(d=d, post = `post_age_5`, dimn=2)
plotagesandknow(d=d, post = `post_age_5`, dimn=4)
dev.off()

png(file = "4_Outputs/plots/supplementary_figures/K3_dim.png", width = 700, height = 300)
par(mfrow = c(1,3),oma = c(0,1,0,0))
plot(apply(post_age_3$K[,,1], 2, mean), apply(post_age_3$K[,,2], 2, mean), pch = 19, col = "cornflowerblue", axes=FALSE, frame.plot=TRUE, xlab = "dimension 1", ylab = "dimension 2")
plot(apply(post_age_3$K[,,2], 2, mean), apply(post_age_3$K[,,3], 2, mean), pch = 19, col = "cornflowerblue", axes=FALSE, frame.plot=TRUE, xlab = "dimension 2", ylab = "dimension 3")
plot(apply(post_age_3$K[,,3], 2, mean), apply(post_age_3$K[,,1], 2, mean), pch = 19, col = "cornflowerblue", axes=FALSE, frame.plot=TRUE, xlab = "dimension 3", ylab = "dimension 1")
dev.off()

#dimrensions contain similar info
par(mar = c(0,0,0,0), mfrow = c(3,3))
plot(apply(post_age_2$K[,,1], 2, mean), apply(post_age_3$K[,,3], 2, mean), pch = 19, col = "100", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_2$K[,,2], 2, mean), apply(post_age_3$K[,,1], 2, mean), pch = 19, col = "100", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_3$K[,,1], 2, mean), apply(post_age_4$K[,,1], 2, mean), pch = 19, col = "50", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_3$K[,,2], 2, mean), apply(post_age_4$K[,,2], 2, mean), pch = 19, col = "50", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_3$K[,,3], 2, mean), apply(post_age_4$K[,,4], 2, mean), pch = 19, col = "50", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_4$K[,,1], 2, mean), apply(post_age_5$K[,,2], 2, mean), pch = 19, col = "30", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_4$K[,,2], 2, mean), apply(post_age_5$K[,,3], 2, mean), pch = 19, col = "30", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_4$K[,,3], 2, mean), apply(post_age_5$K[,,4], 2, mean), pch = 19, col = "30", axes=FALSE, frame.plot=TRUE)
plot(apply(post_age_4$K[,,4], 2, mean), apply(post_age_5$K[,,5], 2, mean), pch = 19, col = "30", axes=FALSE, frame.plot=TRUE)
par(mar = c(5,2,2,2) + 0.1)

##################################################
#Results from models fitted on a single data type#
##################################################

png(file = "4_Outputs/plots/supplementary_figures/eachdata_1dimensions.png", width = 800, height = 380)
par(mfrow = c(1,3),oma = c(0,1,0,0))
plotagesandknow(d=d, post = post_age_l1, 1, maintitle = "a - Freelist")
plotagesandknow(d=d, post = post_age_q1, 1, maintitle = "b - Questionnaire")
plotagesandknow(d=d, post = post_age_r1, 1, maintitle = "c - Image recognition")
dev.off()

png(file = "4_Outputs/plots/supplementary_figures/l_3dimensions.png", width = 700, height = 300)
par(mfrow = c(1,3),oma = c(0,1,0,0))
plotagesandknow(d=d, post = post_age_l3, 3, maintitle = "a")
plotagesandknow(d=d, post = post_age_l3, 2, maintitle = "b")
plotagesandknow(d=d, post = post_age_l3, 1, maintitle = "c")
dev.off()


png(file = "4_Outputs/plots/supplementary_figures/q_3dimensions.png", width = 700, height = 300)
par(mfrow = c(1,3),oma = c(0,1,0,0))
plotagesandknow(d=d, post = post_age_q3, 1)
plotagesandknow(d=d, post = post_age_q3, 2)
plotagesandknow(d=d, post = post_age_q3, 3)
dev.off()

png(file = "4_Outputs/plots/supplementary_figures/r_3dimensions.png", width = 700, height = 300)
par(mfrow = c(1,3),oma = c(0,1,0,0))
plotagesandknow(d=d, post = post_age_r3, 1)
plotagesandknow(d=d, post = post_age_r3, 2)
plotagesandknow(d=d, post = post_age_r3, 3)
dev.off()
par( mfrow = c(1,3))

png(file = "4_Outputs/plots/supplementary_figures/K_by_qn.png", width = 700, height = 300)
par(mfrow = c(1,3),oma = c(0,1,0,0))
plot(apply(post_age_l1$K, 2, mean), apply(post_age_q1$K, 2, mean), 
     pch = 19 , col = "cornflowerblue", xlab = "Knowledge in freelist", ylab = "Knowledge in questionnaire" , cex = 1.5, cex.lab = 1.5)
plot(apply(post_age_l1$K, 2, mean), apply(post_age_r1$K, 2, mean), 
     pch = 19 , col = "cornflowerblue", xlab = "Knowledge in freelist", ylab = "Knowledge in image recognition", cex = 1.5, cex.lab = 1.5  )
plot(apply(post_age_q1$K, 2, mean), apply(post_age_r1$K, 2, mean), 
     pch = 19 , col = "cornflowerblue", xlab = "Knowledge in questionnaire", ylab = "Knowledge in image recognition", cex = 1.5, cex.lab = 1.5  )
dev.off()

