library(rethinking)



###############
##Check results

#if the results have been saved under different biglist, change each of them

#observe if the age and 'real' knowledge correlate. It checks the function creating the knowledge
plot(biglist$A,biglist$K_1)
plot(biglist$A,biglist$K_2)
plot(biglist$A,biglist$K_3)

ggplot(  )+
  geom_point(aes( x = biglist$A, y = biglist$K_1))+
  xlab("Age")+
  ylab("Knowledge")

#See if 'real' knowledge and number of correct answers correlate. It checks the generative function for correct answers.
plot(biglist$K_1, biglist$y)
plot(biglist$K_2, biglist$y)
plot(biglist$K_3, biglist$y)

#see the curves of all the questions. It checks the values g and b (discrimination and difficulty)
plot(NULL,xlim=c(-4,4),ylim=c(0,1))
for ( i in 1:biglist$M ) curve( inv_logit(biglist$g[i]*(x-biglist$b[i])) , add=TRUE )


#estracts the values of K0 from the results of the model and controls with 'real' knowledge. Not expected to correlate
K0_est <- apply( post$K0 , 2 , mean )
K0_CI <- apply( post$K0 , 2 , PI )
plot( biglist$K , K0_est )
for ( i in 1:biglist$N ) lines( c(biglist$K[i],biglist$K[i]) , K0_CI[,i] )

#estracts the values of Ki from the results of the model and controls with 'real' knowledge. It is what we want to reconstruct with the model, so bette be good!
Ki_est <- apply( post$Ki , 2 , mean )
Ki_CI <- apply( post$Ki , 2 , PI )
plot( biglist$K , Ki_est , xlab = "'real' Knowledge")
for ( i in 1:biglist$N ) lines( c(biglist$K[i],biglist$K[i]) , Ki_CI[,i] )

p <- ggplot(  )+
  geom_point(aes( x = biglist$K, y = Ki_est))+
  
  xlab("Knowledge")+
  ylab("Reconstructed knowledge")
for (i in 1:biglist$N) {
  

  p + geom_segment( x = c(biglist$K[i],biglist$K[i]) , y = Ki_CI[i])
}

#estracts the values of b from the results of the model and controls with 'real' b.
b_est <- apply( post$b , 2 , mean)
plot( biglist$b , b_est , xlab = "'real' b")

g_est <- apply( post$g , 2 , mean)
plot( biglist$g , g_est , xlab = "'real' g")



#observe the correlation between age and the predicted knowledge according to the model
plot (biglist$A, Ki_est)


#extracts bA from the model results (as long as N = 100)
prec[(biglist$N + 1),]


