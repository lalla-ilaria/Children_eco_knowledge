#######################
#ORDERED CATEGORICAL A#
#######################
#extract samples
post_msi <- extract.samples(m_ord_min_sint)

post <- post_msi

#explore age effects
plot(apply(post$delta, 2, mean), 1:26, xlim = c(0, 0.3),
     xlab = "Age effect", ylab = "", yaxt='n')
axis(2, 1:26, at = c(1:26), las = 1)
for (i in 1:26) lines(apply(post$delta, 2, PI)[,i], rep(i, each = 2))

#age-sex specific increase in knowledge
png(file = "4_Outputs/plots/age_sex_knowledge.png", width = 600, height = 400)
year_eff <- apply(post$delta_j, 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post$K, 2, mean), xlab = "Age", ylab = "Knowledge", col = dot_col, main = "Age and sex specific increase in knowledge" )
for (i in 1:50) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,,1] * year_eff[,i], type = "l", col = col.alpha( 'darkblue', alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,,2] * year_eff[,i], type = "l", col = col.alpha( 'darkred', alpha = 0.1))
}
dev.off()



############
#ACTIVITIES#
############
post_ao <- extract.samples(m_act_o) #extract samples
post <- post_ao

#activities effect
png(file = "4_Outputs/plots/activities.png", width = 1000, height = 400)
par( mfrow = c( 1, 2))
#age-sex specific knowledge with activities
year_eff <- apply(post$delta_j, 1, cumsum)
plot(d$A[ d$A <= 50 ], apply(post$K, 2, mean), xlab = "Age", ylab = "Knowledge", col = dot_col , main = "a")
for (i in 1:50) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,,1] * year_eff[,i], type = "l", col = col.alpha( 'darkblue', alpha = 0.1))
}
for (i in 1:50) {
  lines(1:nrow(year_eff),  post$mA[i] + post$bA[i,,2] * year_eff[,i], type = "l", col = col.alpha( 'darkred', alpha = 0.1))
}

plot(apply(post$aAM, 2, mean), 1:10, xlim = c(-1.6, 1.5),
     xlab = "Activity effect", ylab = "", yaxt='n', main = "b")
axis(2, colnames(d$am), at = c(1:10), las = 1)
for (i in 1:10) lines(apply(post$aAM, 2, PI)[,i], rep(i, each = 2))
abline(v = 0)

dev.off()