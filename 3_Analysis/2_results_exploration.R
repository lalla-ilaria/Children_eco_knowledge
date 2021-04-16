#ORDERED CATEGORICAL A
post <- extract.samples(m_ord)

Ks <- apply(post$K, 2, mean)

plot(d$A, Ks)

plot (precis(m_ord, 2, pars = "delta_j"))

year_eff <- apply(post$delta_j, 1, cumsum)
plot(1:nrow(year_eff), year_eff[,1], type = "l")
for (i in 1:ncol(year_eff)) {
 lines(1:nrow(year_eff), year_eff[,i], type = "l", col = col.alpha( 'black', alpha = 0.1))
}



#check questions
a_ls <- apply(post$a_l, 2, mean)
b_ls <- apply(post$b_l, 2, mean)


curve(inv_logit(a_ls[1] * ( x - b_ls[1])), xlim = c(-7, 5), col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls)){
  curve(inv_logit(a_ls[i] * ( x - b_ls[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks, rep(0.5, length(Ks)),col = col.alpha("cornflowerblue", 0.7))  
