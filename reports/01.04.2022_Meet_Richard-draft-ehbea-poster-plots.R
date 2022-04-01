library(rethinking)
library(rlist)
source("1_simulation/1b_simulation.R")
real_data <- list.load("2_data_preparation/processed_data.RData")
trapcol <- "#5ca81e"
shellcol <-  "#ea5914"
  
############################################################################
#AGE ONLY
############################################################################
#Real data
dc_shellppl <- real_data$shell_ppl[complete.cases(real_data$shell_ppl$age),]
dc_shells <- real_data$shells[which(real_data$shells$anonymeID %in% dc_shellppl$anonymeID),]
dat <- list(
  N = nrow(dc_shellppl),
  M = nrow(dc_shells),
  A = dc_shellppl$age[order(dc_shellppl$anonymeID)] / mean(dc_shellppl$age),
  R = as.numeric(dc_shells$returns),
  L = dc_shells$lenght_min/mean(dc_shells$lenght_min),
  ID_ind= as.integer(as.factor(as.character(dc_shells$anonymeID)))
)

m_r <- cstan( file= "models/Returns_ind.stan" , data=dat , chains=3, cores = 3 )

dc_trapppl <- real_data$trap_ppl[complete.cases(real_data$trap_ppl$age),]
dc_traps <- real_data$traps[which(real_data$traps$anonymeID %in% dc_trapppl$anonymeID),]
dat <- list(
  N = nrow(dc_trapppl),
  M = nrow(dc_traps),
  A = dc_trapppl$age[order(dc_trapppl$anonymeID)] / mean(dc_trapppl$age),
  S = as.numeric(dc_traps$success),
  L = dc_traps$lenght_hour/mean(dc_traps$lenght_hour),
  ID_ind= as.integer(as.factor(as.character(dc_traps$anonymeID)))
)
m_s <- cstan( file= "models/Success_ind.stan" , data=dat , chains=3, cores = 3 )

post_r <- extract.samples(m_r)
post_s <- extract.samples(m_s)

#plot
plot(NULL, xlim = c(0,30), ylim = c(0,1), xlab = "Age", ylab = "Proportion improvement")
phi <- (1-exp(-median(post_r$beta_a) * seq(0,3,0.1)  )) ^ median(post_r$gamma_a)
lines( seq(0,3,0.1) * mean(dc_shellppl$age),  phi, col = col.alpha(shellcol, 1), lwd = 2)
phi <-  (1-exp(-median(post_s$beta_a) * seq(0,3,0.1)  )) ^ median(post_s$gamma_a)
lines( seq(0,3,0.1) * mean(dc_trapppl$age),  phi, col = col.alpha(trapcol, 1), lwd = 2)

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.1))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.1))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.15))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.15))


for(i in 1:30){
  phi <- (1-exp(-post_r$beta_a[i] * seq(0,3,0.1)  )) ^ post_r$gamma_a[i]
  lines( seq(0,3,0.1) * mean(dc_shellppl$age),  phi, col = col.alpha(shellcol, 0.3))
}
for(i in 1:30){
  phi <-  (1-exp(-post_s$beta_a[i] * seq(0,3,0.1)  )) ^ post_s$gamma_a[i]
  lines( seq(0,3,0.1) * mean(dc_trapppl$age),  phi, col = col.alpha(trapcol, 0.3))
}


############################################################################
#KNOWLEDGE 
############################################################################
#Real data
dc_shellppl <- real_data$shell_ppl[complete.cases(real_data$shell_ppl$knowledge),]
dc_shells <- real_data$shells[which(real_data$shells$anonymeID %in% dc_shellppl$anonymeID),]
dat <- list(
  N = nrow(dc_shellppl),
  M = nrow(dc_shells),
  A = dc_shellppl$age[order(dc_shellppl$anonymeID)] / mean(dc_shellppl$age),
  K = dc_shellppl$knowledge[order(dc_shellppl$anonymeID)] / mean(dc_shellppl$knowledge),
  R = as.numeric(dc_shells$returns),
  L = dc_shells$lenght_min/mean(dc_shells$lenght_min),
  ID_ind= as.integer(as.factor(as.character(dc_shells$anonymeID)))
)
m_rk <- cstan( file= "models/Returns_k.stan" , data=dat , chains=3, cores = 3 )

dc_trapppl <- real_data$trap_ppl[complete.cases(real_data$trap_ppl$knowledge),]
dc_traps <- real_data$traps[which(real_data$traps$anonymeID %in% dc_trapppl$anonymeID),]
dat <- list(
  N = nrow(dc_trapppl),
  M = nrow(dc_traps),
  A = dc_trapppl$age[order(dc_trapppl$anonymeID)] / mean(dc_trapppl$age),
  K = dc_trapppl$knowledge[order(dc_trapppl$anonymeID)] / mean(dc_trapppl$knowledge),
  S = as.numeric(dc_traps$success),
  L = dc_traps$lenght_hour/mean(dc_traps$lenght_hour),
  ID_ind= as.integer(as.factor(as.character(dc_traps$anonymeID)))
)
m_sk <- cstan( file= "models/Success_k.stan" , data=dat , chains=3, cores = 3 )

post_r <- extract.samples(m_rk)
post_s <- extract.samples(m_sk)

#plot age
plot(NULL, xlim = c(0,30), ylim = c(0,1), xlab = "Age", ylab = "Proportion improvement")
phi <- (1-exp(-median(post_r$beta_a) * seq(0,3,0.1)  )) ^ median(post_r$gamma_a)
lines( seq(0,3,0.1) * mean(dc_shellppl$age),  phi, col = col.alpha(shellcol, 1), lwd = 2)
phi <-  (1-exp(-median(post_s$beta_a) * seq(0,3,0.1)  )) ^ median(post_s$gamma_a)
lines( seq(0,3,0.1) * mean(dc_trapppl$age),  phi, col = col.alpha(trapcol, 1), lwd = 2)

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.1))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.1))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.15))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.15))


for(i in 1:30){
  phi <- (1-exp(-post_r$beta_a[i] * seq(0,3,0.1)  )) ^ post_r$gamma_a[i]
  lines( seq(0,3,0.1) * mean(dc_shellppl$age),  phi, col = col.alpha(shellcol, 0.3))
}
for(i in 1:30){
  phi <-  (1-exp(-post_s$beta_a[i] * seq(0,3,0.1)  )) ^ post_s$gamma_a[i]
  lines( seq(0,3,0.1) * mean(dc_trapppl$age),  phi, col = col.alpha(trapcol, 0.3))
}

#plot knowledge
plot(NULL, xlim = c(0,3), ylim = c(0,1), xlab = "Knowledge", ylab = "Proportion improvement")
phi <- (1-exp(-median(post_r$beta_k) * seq(0,3,0.1)  )) ^ median(post_r$gamma_k)
lines( seq(0,3,0.1) ,  phi, col = col.alpha(shellcol, 1), lwd = 2)
phi <-  (1-exp(-median(post_s$beta_k) * seq(0,3,0.1)  )) ^ median(post_s$gamma_k)
lines( seq(0,3,0.1) ,  phi, col = col.alpha(trapcol, 1), lwd = 2)

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_k * x )) ^ post_r$gamma_k, 0.95) )
shade(mu_phi, seq(0,3,0.1), col = col.alpha(shellcol, 0.1))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_k * x )) ^ post_s$gamma_k, 0.95) )
shade(mu_phi, seq(0,3,0.1), col = col.alpha(trapcol, 0.1))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_k * x )) ^ post_r$gamma_k, 0.5) )
shade(mu_phi, seq(0,3,0.1), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_k * x )) ^ post_s$gamma_k, 0.5) )
shade(mu_phi, seq(0,3,0.1), col = col.alpha(trapcol, 0.15))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_k * x )) ^ post_r$gamma_k, 0.3) )
shade(mu_phi, seq(0,3,0.1), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_k * x )) ^ post_s$gamma_k, 0.3) )
shade(mu_phi, seq(0,3,0.1), col = col.alpha(trapcol, 0.15))


for(i in 1:30){
  phi <- (1-exp(-post_r$beta_k[i] * seq(0,3,0.1)  )) ^ post_r$gamma_k[i]
  lines( seq(0,3,0.1) ,  phi, col = col.alpha(shellcol, 0.3))
}
for(i in 1:30){
  phi <-  (1-exp(-post_s$beta_k[i] * seq(0,3,0.1)  )) ^ post_s$gamma_k[i]
  lines( seq(0,3,0.1) ,  phi, col = col.alpha(trapcol, 0.3))
}

############################################################################
#BODY
############################################################################
#Real data
dc_shellppl <- real_data$shell_ppl[complete.cases(real_data$shell_ppl$height&real_data$shell_ppl$age),]
dc_shells <- real_data$shells[which(real_data$shells$anonymeID %in% dc_shellppl$anonymeID),]
dat <- list(
  N = nrow(dc_shellppl),
  M = nrow(dc_shells),
  A = dc_shellppl$age[order(dc_shellppl$anonymeID)] / mean(dc_shellppl$age),
  B = dc_shellppl$height[order(dc_shellppl$anonymeID)] / mean(dc_shellppl$height),
  R = as.numeric(dc_shells$returns),
  L = dc_shells$lenght_min/mean(dc_shells$lenght_min),
  ID_ind= as.integer(as.factor(as.character(dc_shells$anonymeID)))
)
m_rb <- cstan( file= "models/Returns_b.stan" , data=dat , chains=3, cores = 3 )

dc_trapppl <- real_data$trap_ppl[complete.cases(real_data$trap_ppl$height),]
dc_traps <- real_data$traps[which(real_data$traps$anonymeID %in% dc_trapppl$anonymeID),]
dat <- list(
  N = nrow(dc_trapppl),
  M = nrow(dc_traps),
  A = dc_trapppl$age[order(dc_trapppl$anonymeID)] / mean(dc_trapppl$age),
  B = dc_trapppl$height[order(dc_trapppl$anonymeID)] / mean(dc_trapppl$height),
  S = as.numeric(dc_traps$success),
  L = dc_traps$lenght_hour/mean(dc_traps$lenght_hour),
  ID_ind= as.integer(as.factor(as.character(dc_traps$anonymeID)))
)
m_sb <- cstan( file= "models/Success_b.stan" , data=dat , chains=3, cores = 3 )

post_s <- extract.samples(m_sb)
post_r <- extract.samples(m_rb)

#plot age
plot(NULL, xlim = c(0,30), ylim = c(0,1), xlab = "Age", ylab = "Proportion improvement")
phi <- (1-exp(-median(post_r$beta_a) * seq(0,3,0.1)  )) ^ median(post_r$gamma_a)
lines( seq(0,3,0.1) * mean(dc_shellppl$age),  phi, col = col.alpha(shellcol, 1), lwd = 2)
phi <-  (1-exp(-median(post_s$beta_a) * seq(0,3,0.1)  )) ^ median(post_s$gamma_a)
lines( seq(0,3,0.1) * mean(dc_trapppl$age),  phi, col = col.alpha(trapcol, 1), lwd = 2)

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.1))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.1))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.15))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.15))


for(i in 1:30){
  phi <- (1-exp(-post_r$beta_a[i] * seq(0,3,0.1)  )) ^ post_r$gamma_a[i]
  lines( seq(0,3,0.1) * mean(dc_shellppl$age),  phi, col = col.alpha(shellcol, 0.3))
}
for(i in 1:30){
  phi <-  (1-exp(-post_s$beta_a[i] * seq(0,3,0.1)  )) ^ post_s$gamma_a[i]
  lines( seq(0,3,0.1) * mean(dc_trapppl$age),  phi, col = col.alpha(trapcol, 0.3))
}

#plot body
plot(NULL, xlim = c(0,160), ylim = c(0,1), xlab = "Height", ylab = "Proportion improvement")
phi <- (1-exp(-median(post_r$beta_b) * seq(0,3,0.1)  )) ^ median(post_r$gamma_b)
lines( seq(0,3,0.1) * mean(dc_shellppl$height),  phi, col = col.alpha(shellcol, 1), lwd = 2)
phi <-  (1-exp(-median(post_s$beta_b) * seq(0,3,0.1)  )) ^ median(post_s$gamma_b)
lines( seq(0,3,0.1) * mean(dc_trapppl$height),  phi, col = col.alpha(trapcol, 1), lwd = 2)

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_b * x )) ^ post_r$gamma_b, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$height), col = col.alpha(shellcol, 0.1))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_b * x )) ^ post_s$gamma_b, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$height), col = col.alpha(trapcol, 0.1))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_b * x )) ^ post_r$gamma_b, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$height), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_b * x )) ^ post_s$gamma_b, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$height), col = col.alpha(trapcol, 0.15))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_b * x )) ^ post_r$gamma_b, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$height), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_b * x )) ^ post_s$gamma_b, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$height), col = col.alpha(trapcol, 0.15))


for(i in 1:30){
  phi <- (1-exp(-post_r$beta_b[i] * seq(0,3,0.1)  )) ^ post_r$gamma_b[i]
  lines( seq(0,3,0.1) * mean(dc_shellppl$height),  phi, col = col.alpha(shellcol, 0.3))
}
for(i in 1:30){
  phi <-  (1-exp(-post_s$beta_b[i] * seq(0,3,0.1)  )) ^ post_s$gamma_b[i]
  lines( seq(0,3,0.1) * mean(dc_trapppl$height),  phi, col = col.alpha(trapcol, 0.3))
}

############################################################################
#EVERYTHING TOGETHER
############################################################################
#Real data
dc_shellppl <- real_data$shell_ppl[complete.cases(real_data$shell_ppl$knowledge),]
dc_shellppl <- dc_shellppl[complete.cases(dc_shellppl$height),]
dc_shells <- real_data$shells[which(real_data$shells$anonymeID %in% dc_shellppl$anonymeID),]
dat_shells <- list(
  N = nrow(dc_shellppl),
  M = nrow(dc_shells),
  A = dc_shellppl$age[order(dc_shellppl$anonymeID)] / mean(dc_shellppl$age),
  K = dc_shellppl$knowledge[order(dc_shellppl$anonymeID)] / mean(dc_shellppl$knowledge),
  B = dc_shellppl$height[order(dc_shellppl$anonymeID)] / mean(dc_shellppl$height),
  R = as.numeric(dc_shells$returns),
  L = dc_shells$lenght_min/mean(dc_shells$lenght_min),
  ID_ind= as.integer(as.factor(as.character(dc_shells$anonymeID)))
)
dat <- dat_shells
m_ra <- cstan( file= "models/Returns_all.stan" , data=dat , chains=3, cores = 3 )
post_r <- extract.samples(m_ra)

dc_trapppl <- real_data$trap_ppl[complete.cases(real_data$trap_ppl$knowledge),]
dc_traps <- real_data$traps[which(real_data$traps$anonymeID %in% dc_trapppl$anonymeID),]
dat_traps <- list(
  N = nrow(dc_trapppl),
  M = nrow(dc_traps),
  A = dc_trapppl$age[order(dc_trapppl$anonymeID)] / mean(dc_trapppl$age),
  K = dc_trapppl$knowledge[order(dc_trapppl$anonymeID)] / mean(dc_trapppl$knowledge),
  B = dc_trapppl$height[order(dc_trapppl$anonymeID)] / mean(dc_trapppl$height),
  S = as.numeric(dc_traps$success),
  L = dc_traps$lenght_hour/mean(dc_traps$lenght_hour),
  ID_ind= as.integer(as.factor(as.character(dc_traps$anonymeID)))
)
dat <- dat_traps
m_sa <- cstan( file= "models/Success_all.stan" , data=dat , chains=3, cores = 3 )
post_s <- extract.samples(m_sa)

#plot age
plot(NULL, xlim = c(0,30), ylim = c(0,1), xlab = "Age", ylab = "Proportion improvement")
phi <- (1-exp(-median(post_r$beta_a) * seq(0,3,0.1)  )) ^ median(post_r$gamma_a)
lines( seq(0,3,0.1) * mean(dc_shellppl$age),  phi, col = col.alpha(shellcol, 1), lwd = 2)
phi <-  (1-exp(-median(post_s$beta_a) * seq(0,3,0.1)  )) ^ median(post_s$gamma_a)
lines( seq(0,3,0.1) * mean(dc_trapppl$age),  phi, col = col.alpha(trapcol, 1), lwd = 2)

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.1))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.95) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.1))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.5) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.15))

mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_r$beta_a * x )) ^ post_r$gamma_a, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(shellcol, 0.15))
mu_phi <-   sapply ( seq(0,3,0.1) , function (x) PI ((1-exp(-post_s$beta_a * x )) ^ post_s$gamma_a, 0.3) )
shade(mu_phi, seq(0,3,0.1)* mean(dc_trapppl$age), col = col.alpha(trapcol, 0.15))


for(i in 1:30){
  phi <- (1-exp(-post_r$beta_a[i] * seq(0,3,0.1)  )) ^ post_r$gamma_a[i]
  lines( seq(0,3,0.1) * mean(dc_shellppl$age),  phi, col = col.alpha(shellcol, 0.3))
}
for(i in 1:30){
  phi <-  (1-exp(-post_s$beta_a[i] * seq(0,3,0.1)  )) ^ post_s$gamma_a[i]
  lines( seq(0,3,0.1) * mean(dc_trapppl$age),  phi, col = col.alpha(trapcol, 0.3))
}

############################################################################
#SIMULATIONS
############################################################################
#Simulate
d<-sim_data(100,300, zero = F)
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A/mean(d$A),
  R = d$R,
  L = d$L/mean(d$L),
  ID_ind = d$ID_ind
)
m_r <- cstan( file= "models/Returns_ind.stan" , data=dat , chains=3, cores = 3 )

d<-sim_data(100,300, zero = T, b_a = 0.2, g_ak = 4)
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A/mean(d$A),
  S = d$S,
  L = d$L/mean(d$L),
  ID_ind = d$ID_ind
)
m_s <- cstan( file= "models/Success_ind.stan" , data=dat , chains=3, cores = 3 )

post_r <- extract.samples(m_r)
post_s <- extract.samples(m_s)


plot(NULL, xlim = c(0,20), ylim = c(0,1))
for(i in 1:150){
  phi <- (1-exp(-post_r$beta_a[i] * seq(0,3,0.1)  )) ^ post_r$gamma_a[i]
  lines( seq(0,3,0.1) * mean(d$A),  phi, col = col.alpha("cornflowerblue", 0.7))
}
for(i in 1:150){
  phi <- (1-exp(-post_s$beta_a[i] * seq(0,3,0.1)  )) ^ post_s$gamma_a[i]
  lines( seq(0,3,0.1) * mean(d$A),  phi, col = col.alpha("lightblue", 0.7))
}
#Knowledge
d<-sim_data(100,300, zero = F)
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A/mean(d$A),
  R = d$R,
  L = d$L/mean(d$L),
  K = d$K/mean(d$K),
  ID_ind = d$ID_ind
)
m_rk <- cstan( file= "models/Returns_k.stan" , data=dat , chains=3, cores = 3 )
post_r <- extract.samples(m_rk)


d<-sim_data(100,300, zero = T, b_a = 0.2, g_ak = 4)
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A/mean(d$A),
  S = d$S,
  L = d$L/mean(d$L),
  K = d$K/mean(d$K),
  ID_ind = d$ID_ind
)

m_sk <- cstan( file= "models/Success_k.stan" , data=dat , chains=3, cores = 3 )
post_s <- extract.samples(m_sk)

#plot
plot(NULL, xlim = c(0,3), ylim = c(0,1))
for(i in 1:150){
  phi <- (1-exp(-post_r$beta_k[i] * seq(0,3,0.1)  )) ^ post_r$gamma_k[i]
  lines( seq(0,3,0.1) ,  phi, col = col.alpha("cornflowerblue", 0.7))
}
phi <- (1-exp(-mean(post_r$beta_k[i]) * seq(0,3,0.1)  )) ^ mean(post_r$gamma_k[i])
lines( seq(0,3,0.1) ,  phi, col = col.alpha("darkblue", 0.7))

for(i in 1:150){
  phi <- (1-exp(-post_s$beta_k[i] * seq(0,3,0.1)  )) ^ post_s$gamma_k[i]
  lines( seq(0,3,0.1) ,  phi, col = col.alpha("lightblue", 0.7))
}
