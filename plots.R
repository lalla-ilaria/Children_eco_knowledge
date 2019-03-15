
#household composition
plot (table(df_ID$household), type= "h", ylab = "number of children per household")                #checks the number of children per household

plot (df_ID$household, df_ID$age, pch = 16, col = ifelse(df_ID$sex==-1, "darkred", "navy"))                              #gets composition of households


#schooling
plot(df_ID$age, df_ID$school_years, col = ifelse(df_ID$sex==-1, "darkred", "navy"), pch = 16, ylab = "number of years spent in school") #check uears of schooling and age, colors indicate sex

#activities
par(mfcol=c(3, 3))
  for (i in 1:9) {
    plot (jitter(df_ID$sex), jitter(matrix_IDactivity[,i]), xlab = "", ylab = "practiced", xaxt='n', yaxt='n', main = paste("activity", i))               #check the effect of sex per activity (change the activity after the comma)
    axis(1, at= c(-1,1), labels= c("sex1", "sex2"), pos= -0.1, lty = 0)
    axis(2, at= c(0,1), labels= c("no", "yes"), pos= -1.2, lty = 0)
  }
par(mfcol=c(1, 1))

act_performed <- apply(matrix_IDactivity, 1, sum)
plot(df_ID$age, act_performed, pch = 16, ylab = "numbber activities performed") #check the effect of age on the number of activities performed
rm(act_performed)

#species, probabilities
par(mfcol=c(2, 4))
for (i in 1:8) {
  plot(sp_curve[i,], xlim = c(5,20), ylim = c(0,1))  
}
par(mfcol=c(1, 1))

#frequency each item is listed by people
ncit <- rep(NA, sp)
    for (i in 1:sp) {
      ncit[i] <- (sum(  df_IDsp$species == i & df_IDsp$known == 1))
    }
  ncit <-ncit[order(ncit, decreasing = TRUE)]
plot(ncit, type = "h", xlab ="species by number cited", ylab = "number times cited")
rm(ncit)


#number of species known by individuals
sp_known <- rep(NA, N)
for (i in 1:N) {
  sp_known [i] <- (sum(  df_IDsp$ID == i & df_IDsp$known == 1))
}
plot(df_ID$age, sp_known)
rm(sp_known)


#plot of species known by individuals
#order data frame to plot individuals by age

#plot species known by individual


df_order <- data.frame(ID = df_ID$ID,
                 age = df_ID$age,
                 sex = df_ID$sex)
df_order <- df_order[rep(seq_len(nrow(df_order)), each = sp),]          #create rows per each species known or not by each individual
df_order <-cbind(df_order, df_IDsp[,2:3])

df_order <- df_order[order(df_order$age),]
df_order$ageorder <- rep(1:N, each = sp)

plot(1, type="n", 
     xlab="species", ylab="individuals by age", 
     xlim = c(1,sp), ylim = c(1,N))

points( x = df_order$species , y = df_order$ageorder, pch = ifelse(df_order$known==1, 16, 1), col = ifelse(df_order$sex==1, "navy", "dark red" ))
rm(df_order)



####plots from models######

#plot values: predicted by the model (both using link and without), real probability to construct data and proportion of species known
plot(1:N, df_ID$prop_kn , xlim = c(1,N), ylim = c(0,1), pch = 16, col = "grey", ylab = "p knowledge")
lines(x = c(1,N), y = c(mean(df_ID$prop_kn),mean(df_ID$prop_kn)), col = "grey")


#model m1.1, intercept only
#retrodict data - use link
p_mu <- matrix( data = NA, nrow = N, ncol = sp)

for (i in 1:N) {
  dat_pred <- list(
    ID = rep(i, sp) 
  )
  p_r <- link(m1.1, data = dat_pred )
  p_mu[i,] <- apply( p_r , 2 , mean ) 
}#N

#average value per person
p_mu1lavg <- apply(p_mu, 1, mean)

#add points
points(1:N, p_mu1lavg, pch = 16 , col = "navy") 

legend(1, 0.2, legend=c("Proportion known - data", "Model probability-intercept only"),
       col=c("grey", "navy"), pch = 16)


#m1.2 add slope for age######
#retrodict data - use link
p_mu <- matrix( data = NA, nrow = N, ncol = sp)

for (i in 1:N) {
  dat_pred <- list(
    ID = rep(i, sp) ,
    age = rep(df_ID$age_st[i], sp)
  )
  p_r <- link(m1.2, data = dat_pred )
  p_mu[i,] <- apply( p_r , 2 , mean ) 
}#N
p_mu2lavg <- apply(p_mu, 1, mean)

#add points
points(1:N, p_mu2lavg, pch = 16 , col = "darkred")

legend(1, 0.2, legend=c("Proportion known - data", "m1.1 intercept only", "m1.2 individuals and age effect"),
       col=c("grey", "navy", "darkred"), pch = 16)



