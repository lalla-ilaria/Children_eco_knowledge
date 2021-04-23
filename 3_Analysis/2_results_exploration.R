#######################
#ORDERED CATEGORICAL A#
#######################
#extract samples
post <- extract.samples(m_ord)

#INDIVIDUALS
############
#plot knowledge estimation by age
Ks <- apply(post$K, 2, mean)
plot(d$A [ d$A <= 50 ], Ks, xlab = "Age", ylab = "Knowledge" )

aKs <- apply(post$aK, 2, mean)
plot( Ks, aKs)
plot(d$A [ d$A <= 50 ], aKs, xlab = "Age", ylab = "Individual effect on knowledge")

#explore age effects
plot (precis(m_ord_s, 2, pars = "delta_j"))

year_eff <- apply(post$delta_j, 1, cumsum)
plot(1:nrow(year_eff), year_eff[,1], type = "l", 
     xlab = "Age", ylab = "Age specific effect on knowledge" )
for (i in 1:ncol(year_eff)) {
  lines(1:nrow(year_eff), year_eff[,i], type = "l", col = col.alpha( 'black', alpha = 0.1))
}



#ITEMS
######
#freelist
a_ls <- apply(post$a_l, 2, mean)
b_ls <- apply(post$b_l, 2, mean)

curve(inv_logit(a_ls[1] * ( x - b_ls[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge in freelists", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls)){
  curve(inv_logit(a_ls[i] * ( x - b_ls[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks, rep(0.5, length(Ks)),col = col.alpha("cornflowerblue", 0.7))  

#questions
a_qs <- apply(post$a_q, 2, mean)
b_qs <- apply(post$b_q, 2, mean)

curve(inv_logit(a_qs[1] * ( x - b_qs[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge in questions", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_qs)){
  curve(inv_logit(a_qs[i] * ( x - b_qs[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks, rep(0.5, length(Ks)),col = col.alpha("cornflowerblue", 0.7))  

#check image recognition
a_rs <- apply(post$a_r, 2, mean)
b_rs <- apply(post$b_r, 2, mean)

curve(inv_logit(a_rs[1] * ( x - b_rs[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge in image recognition", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_rs)){
  curve(inv_logit(a_rs[i] * ( x - b_rs[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks, rep(0.5, length(Ks)),col = col.alpha("cornflowerblue", 0.7))  

############
#DIMENSIONS#
############
post_d <- extract.samples(m_d3)

#check the correlation between dimensions
par( mfrow = c( 1,3))
#knowledge
plot(apply( post_d$K[,,1], 2, mean), apply( post_d$K[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2")
plot(apply( post_d$K[,,1], 2, mean), apply( post_d$K[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3")
plot(apply( post_d$K[,,2], 2, mean), apply( post_d$K[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3")
#discrimination
plot(apply( post_d$a_l[,,1], 2, mean), apply( post_d$a_l[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2")
plot(apply( post_d$a_l[,,1], 2, mean), apply( post_d$a_l[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3")
plot(apply( post_d$a_l[,,2], 2, mean), apply( post_d$a_l[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3")
#difficulty
plot(apply( post_d$b_l[,,1], 2, mean), apply( post_d$b_l[,,2], 2, mean),
     xlab = "K 1", ylab = "K 2")
plot(apply( post_d$b_l[,,1], 2, mean), apply( post_d$b_l[,,3], 2, mean),
     xlab = "K 1", ylab = "K 3")
plot(apply( post_d$b_l[,,2], 2, mean), apply( post_d$b_l[,,3], 2, mean),
     xlab = "K 2", ylab = "K 3")

#check effect of age
precis(m_d[[1]], pars = "bA")
precis(m_d[[2]], pars = "bA")
precis(m_d[[3]], pars = "bA")


#check knowledge and questions parameters
#1st dimension
Ks_1 <- apply(post_d$K[,,1], 2, mean)
a_ls_1 <- apply(post_d$a_l[,,1], 2, mean)
b_ls_1 <- apply(post_d$b_l[,,1], 2, mean)
curve(inv_logit(a_ls_1[1] * ( x - b_ls_1[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 1", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls_1)){
  curve(inv_logit(a_ls_1[i] * ( x - b_ls_1[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks_1, rep(0.5, length(Ks_1)),col = col.alpha("cornflowerblue", 0.7))  

#2nd dimension
Ks_2 <- apply(post_d$K[,,2], 2, mean)
a_ls_2 <- apply(post_d$a_l[,,2], 2, mean)
b_ls_2 <- apply(post_d$b_l[,,2], 2, mean)
curve(inv_logit(a_ls_2[1] * ( x - b_ls_2[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 2", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls_2)){
  curve(inv_logit(a_ls_2[i] * ( x - b_ls_2[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks_2, rep(0.5, length(Ks_2)),col = col.alpha("cornflowerblue", 0.7))  

#3rd dimension
Ks_3 <- apply(post_d$K[,,3], 2, mean)
a_ls_3 <- apply(post_d$a_l[,,3], 2, mean)
b_ls_3 <- apply(post_d$b_l[,,3], 2, mean)
curve(inv_logit(a_ls_3[1] * ( x - b_ls_3[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 3", ylab = "p correct answer",
      col = col.alpha("lightblue", 0.2))
for(i in 1: length(a_ls_3)){
  curve(inv_logit(a_ls_3[i] * ( x - b_ls_3[i])), col = col.alpha("lightblue", 0.2), add = TRUE)
}
points(Ks_3, rep(0.5, length(Ks_1)),col = col.alpha("cornflowerblue", 0.7))  

par( mfrow = c( 1, 1))

#coloring by type of question
unique (all_items$type)

d$color_l <- ifelse( is.na(d$type_l), "yellow",
             ifelse( d$type_l  == "N", "darkorange",
             ifelse( d$type_l  == "S", "darkblue",
             ifelse( d$type_l  == "W", "darkred",
             ifelse( d$type_l  == "D", "orchid",
             ifelse( d$type_l  == "M", "darkgreen", 
             NA))))))

#1st dimension
Ks_1 <- apply(post_d$K[,,1], 2, mean)
a_ls_1 <- apply(post_d$a_l[,,1], 2, mean)
b_ls_1 <- apply(post_d$b_l[,,1], 2, mean)
curve(inv_logit(a_ls_1[1] * ( x - b_ls_1[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 1", ylab = "p correct answer",
      col = col.alpha( d$color_l, 0.2))
for(i in 1: length(a_ls_1)){
  curve(inv_logit(a_ls_1[i] * ( x - b_ls_1[i])), 
        col = col.alpha(d$color_l[i], 0.2), add = TRUE)
}
points(Ks_1, rep(0.5, length(Ks_1)), col = col.alpha("cornflowerblue", 0.7))  

#2nd dimension
Ks_2 <- apply(post_d$K[,,2], 2, mean)
a_ls_2 <- apply(post_d$a_l[,,2], 2, mean)
b_ls_2 <- apply(post_d$b_l[,,2], 2, mean)
curve(inv_logit(a_ls_2[1] * ( x - b_ls_2[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 2", ylab = "p correct answer",
      col = col.alpha(d$color_l, 0.2))
for(i in 1: length(a_ls_2)){
  curve(inv_logit(a_ls_2[i] * ( x - b_ls_2[i])), 
        col = col.alpha(d$color_l[i], 0.2), add = TRUE)
}
points(Ks_2, rep(0.5, length(Ks_2)),col = col.alpha("cornflowerblue", 0.7))  

#3rd dimension
Ks_3 <- apply(post_d$K[,,3], 2, mean)
a_ls_3 <- apply(post_d$a_l[,,3], 2, mean)
b_ls_3 <- apply(post_d$b_l[,,3], 2, mean)
curve(inv_logit(a_ls_3[1] * ( x - b_ls_3[1])), 
      xlim = c(-7, 5), ylim = c(0, 1), 
      xlab = "knowledge dimension 3", ylab = "p correct answer",
      col = col.alpha(d$color_l, 0.2))
for(i in 1: length(a_ls_3)){
  curve(inv_logit(a_ls_3[i] * ( x - b_ls_3[i])), 
        col = col.alpha(d$color_l[i], 0.2), add = TRUE)
}
points(Ks_3, rep(0.5, length(Ks_3)),col = col.alpha("cornflowerblue", 0.7))  
