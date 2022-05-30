#Set the stage for plotting figures
if( !exists( "plotagesandknow", mode = "function")) source( "4_Outputs/0_set_the_plot_stage.R" )


#######################
#ORDERED CATEGORICAL A#
#######################
#age-sex specific increase in knowledge

png(file = "4_Outputs/plots/age_sex_knowledge.png", 
    width = 12, height = 9, units = "cm", pointsize = 6,  res = 300)
plotagesandknow(d=d, post = post_age_1, dimn=1)
dev.off()

#dimensions
png(file = "4_Outputs/plots/age_sex_dimensions.png", 
    width = 18, height = 9, units = "cm", pointsize = 9,  res = 300)
par(mfrow = c(1,3),mgp = c(1.5, 0.5, 0), mar = c(2.5, 2.5, 2, 1) + 0.1)
plotagesandknow(d=d, post = `post_age_3`, dimn=1, maintitle = "General Knowledge")
plotagesandknow(d=d, post = `post_age_3`, dimn=3, maintitle = "Male-Specific Knowledge")
plotagesandknow(d=d, post = `post_age_3`, dimn=2, maintitle = "Other Knowledge")
dev.off()


############
#ACTIVITIES#
############

#effect of activities
png(file = "4_Outputs/plots/activities_year_gained.png", 
    width = 12, height = 10, units = "cm", pointsize = 10,  res = 300)
par(mar = c(5,5,2,2) + 0.1, mfrow = c(1,1))
actdiff <- plotact(post = `post_act_1`)
dev.off()

#calculate tail of the distributions below -20 years
for (i in 1:d$C){
  print ( paste ( sum (actdiff[[i]] <= -20) / 500, "percentile of the distribution for activity", i , "below -20 years"))
}
    
par(mfrow = c(1,1))