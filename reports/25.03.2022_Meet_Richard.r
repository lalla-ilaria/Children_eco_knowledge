nsamp <-100
#PRIOR PREDICTIVE SIMULATION
#Set trait values for age and possible times
AGE <- seq(0,3, 0.1) #trait
L <- c(0, 0.1, 1, 3) #trait
#create matrixes to save individual and trip effects
phi <- matrix(NA, nsamp, length(AGE))
psi <- rep(NA, nsamp)
#SIMULATED PRIORS
#general intercept
alpha <- rnorm(nsamp, 0, 1)#prior
#trip
lambda <-  rexp(nsamp,1)
#individual_age
beta_a <- rlnorm(nsamp, 0, 1)#prior
gamma_a <- rlnorm(nsamp, 0, 1)#prior
#sigma lognormal
sigma <- rexp(nsamp, 1)
#plot prior predictive simulation
plot(NULL, xlim = c(0,3), ylim = c (0,10), 
     xlab = "age", ylab = "returns amount")
#calculate per sample
for(i in 1:nsamp){
  phi[i,] <- (1-exp(-beta_a[i] * AGE  )) ^ gamma_a[i]
  psi[i] <-   lambda[i] * log (L[2])
  R <- exp ( alpha[i] + log(phi[i,]) + psi[i] + ((sigma[i]^2) /2))
  lines( AGE,  R, col = col.alpha("cornflowerblue", 0.7))
}
###########################

#all good!

#Now I'm trying to add things to the individual 
#the total effect of the individual depends from knowledge, body and other traits 
#that vary with age plus some other things that don't vary with age
#so let's keep all the things we defined above, plus let's create a knowledge effect
#individual_knowledge
beta_ak <- rlnorm(nsamp, 0, 1)#prior
gamma_ak <- rlnorm(nsamp, 0, 1)#prior
beta_k <- rlnorm(nsamp, 0, 1)#prior
gamma_k <- rlnorm(nsamp, 0, 1)#prior
K <- matrix(NA, nsamp, length(AGE)) 
for(i in 1:nsamp){
  K[i,] <- (1-exp(-beta_ak[i]*AGE))^gamma_ak[i] #trait
}

#Individual level random effects
id_v <- rnorm(nsamp, 0, 0.3)

#and plot again returns curves with the effect of knowledge again
plot(NULL, xlim = c(0,3), ylim = c (0,10), 
     xlab = "age", ylab = "returns amount")

for(i in 1:nsamp){
  phi[i,] <- exp(id_v[i]) * ((1-exp(-beta_a[i] * AGE  )) ^ gamma_a[i]+
                   (1-exp(-beta_k[i] * K[i,])) ^ gamma_k[i])
  psi[i] <-   lambda[i] * log (L[2])
  R <- exp ( alpha[i] + log(phi[i,]) + psi[i] + ((sigma[i]^2) /2))
  lines( AGE,  R, col = col.alpha("lightblue", 0.7))
}

#REAL DATA
dc_shellppl <- real_data$shell_ppl[complete.cases(real_data$shell_ppl$age),]
dc_shells <- real_data$shells[which(real_data$shells$anonymeID %in% d_complete_ppl$anonymeID),]
dat <- list(
  N = nrow(dc_shellppl),
  M = nrow(dc_shells),
  A = dc_shellppl$age[order(dc_shellppl$anonymeID)] / mean(dc_shellppl$age),
  R = as.numeric(dc_shells$returns)/100,
  L = dc_shells$lenght_min/mean(dc_shells$lenght_min),
  ID_ind= as.integer(as.factor(as.character(dc_shells$anonymeID)))
)

m_r <- cstan( file= "models/Returns_ind.stan" , data=dat , chains=3, cores = 3 )
post_r <- extract.samples(m_r)


dc_trapppl <- real_data$trap_ppl[complete.cases(real_data$trap_ppl$age),]
dc_traps <- real_data$traps[which(real_data$traps$anonymeID %in% d_complete_ppl$anonymeID),]
dat <- list(
  N = nrow(dc_trapppl),
  M = nrow(dc_traps),
  A = dc_trapppl$age[order(dc_trapppl$anonymeID)] / mean(dc_trapppl$age),
  S = as.numeric(dc_traps$success),
  L = dc_traps$lenght_hour/mean(dc_traps$lenght_hour),
  ID_ind= as.integer(as.factor(as.character(dc_traps$anonymeID)))
)

m_s <- cstan( file= "models/Success_ind.stan" , data=dat , chains=3, cores = 3 )
post_s <- extract.samples(m_s)


precis(m_s)
