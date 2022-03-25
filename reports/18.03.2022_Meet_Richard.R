library(rethinking)
library(rlist)
source("1_simulation/1b_simulation.R")

#CONSTRUCT MODEL FROM PRINCIPLES?
#Returns depend from individual level and trip level characteristics
# R <- rlnorm( m , sigma )
# m <- alpha + phi + psi

#TRIP characteristics include time L
# psi <-   lambda * log (L)

#INDIVIDUAL characteristics include age
#functional form as in chapter 9 solutions (but without old age)
# phi <- beta_ind + log(1-exp(-beta_a * AGE  )) * gamma_a
#####################
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
alpha <- rnorm(nsamp, 0, 0.1)#prior
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

#and plot again returns curves with the effect of knowledge again
plot(NULL, xlim = c(0,3), ylim = c (0,10), 
     xlab = "age", ylab = "returns amount")

for(i in 1:nsamp){
  phi[i,] <- (1-exp(-beta_a[i] * AGE  )) ^ gamma_a[i]+
                   (1-exp(-beta_k[i] * K[i,])) ^ gamma_k[i]
  psi[i] <-   lambda[i] * log (L[2])
  R <- exp ( alpha[i] + log(phi[i,]) + psi[i] + ((sigma[i]^2) /2))
  lines( AGE,  R, col = col.alpha("cornflowerblue", 0.7))
}

#Each curve is the result of the sum of the effects of age (representing 
#unmeasured age related traits) and knowledge, which in itself varies with age
#We can see it plotting a single curve and the separate effects of knowledge and age only
i<-2
plot(NULL, xlim = c(0,3), ylim = c (0,10), 
     xlab = "age", ylab = "returns amount")
R <- exp ( alpha[i] + log(phi[i,]) + ((sigma[i]^2) /2))
lines( AGE,  R, col = col.alpha("darkblue", 0.7))

#age only
phi[i,] <- (1-exp(-beta_a[i] * AGE  )) ^ gamma_a[i]
R <- exp ( alpha[i] + log(phi[i,]) + ((sigma[i]^2) /2))
lines( AGE,  R, col = col.alpha("lightblue", 0.7))
#knwoledgeonly
phi[i,] <- (1-exp(-beta_k[i] * K[i,]  )) ^ gamma_k[i]
R <- exp ( alpha[i] + log(phi[i,]) + ((sigma[i]^2) /2))
lines( AGE,  R, col = col.alpha("cornflowerblue", 0.7))


#and how about success?
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
alpha <- rexp(nsamp, 1)#prior
#trip
lambda <-  rexp(nsamp,1)
#individual_age
beta_a <- rlnorm(nsamp, 0, 1)#prior
gamma_a <- rlnorm(nsamp, 0, 1)#prior
#individual knowledge
beta_ak <- rlnorm(nsamp, 0, 1)#prior
gamma_ak <- rlnorm(nsamp, 0, 1)#prior
beta_k <- rlnorm(nsamp, 0, 1)#prior
gamma_k <- rlnorm(nsamp, 0, 1)#prior
K <- matrix(NA, nsamp, length(AGE)) 
for(i in 1:nsamp){
  K[i,] <- (1-exp(-beta_ak[i]*AGE))^gamma_ak[i] #trait
}#sigma lognormal
sigma <- rexp(nsamp, 1)
#plot prior predictive simulation
plot(NULL, xlim = c(0,3), ylim = c (0,1), 
     xlab = "age", ylab = "returns amount")
#calculate per sample
for(i in 1:nsamp){
  phi[i,] <- (1-exp(-beta_a[i] * AGE  )) ^ gamma_a[i]+
                   (1-exp(-beta_k[i] * K[i,])) ^ gamma_k[i]
  psi[i] <- (L[2])^lambda[i]
  p <- 1 - exp ( - alpha[i] * phi[i,] * psi[i] ) #1-e^{-\lambda x}
  #S <- bernoulli
  lines( AGE,  p, col = col.alpha("cornflowerblue", 0.7))
}
#alpha needs to be positive?!
plot(NULL, xlim = c(0,3), ylim = c (0,1), 
     xlab = "age", ylab = "returns amount")
p <- 1 - exp ( alpha[i] * phi[i,] * psi[i] )
lines( AGE,  p, col = col.alpha("darkblue", 0.7))

#age only
phi[i,] <- (1-exp(-beta_a[i] * AGE  )) ^ gamma_a[i]
p <- 1 - exp ( alpha[i] * phi[i,] * psi[i] )
lines( AGE,  p, col = col.alpha("cornflowerblue", 0.7))
#knwoledgeonly
phi[i,] <- (1-exp(-beta_k[i] * K[i,]  )) ^ gamma_k[i]
p <- 1 - exp ( alpha[i] * phi[i,] * psi[i] )
lines( AGE,  p, col = col.alpha("lightblue", 0.7))


#NOW
#1) add body
#2) other traits + individual effects not related to age

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

m_fit <- cstan( file= "models/Returns.stan" , data=dat , chains=3, cores = 3 )


#plot
post <- extract.samples(m_fit)
plot(d$A[d$ID_ind], d$R)
for(i in 1:550){
  phi <- (1-exp(-post$beta_a[i] * seq(0,3,0.1)  )) ^ post$gamma_a[i]
  R <- exp ( post$alpha[i] + phi + ((post$sigma[i]^2) /2))
  lines( seq(0,3,0.1) * mean(d$A),  R, col = col.alpha("cornflowerblue", 0.7))
}

d<-sim_data(100,300, zero = F, b_a = 3, g_ak = 3)
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A/mean(d$A),
  R = d$R,
  L = d$L/mean(d$L),
  K = d$K/mean(d$K),
  ID_ind = d$ID_ind
)

m_fit <- cstan( file= "models/Returns.stan" , data=dat , chains=3, cores = 3 )
post <- extract.samples(m_fit)

plot(NULL, xlim = c(0,3), ylim = c (0,1), 
     xlab = "age", ylab = "returns amount")
for(i in 1:100){
  phi <- (1-exp(-post$beta_a[i] * seq(0,3,0.1)  )) ^ post$gamma_a[i]
  lines( seq(0,3,0.1) ,  phi, col = col.alpha("cornflowerblue", 0.7))
}


d<-sim_data(100,300, zero = F, b_a = 1, g_ak = 1)
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A/mean(d$A),
  R = d$R,
  L = d$L/mean(d$L),
  K = d$K/mean(d$K),
  ID_ind = d$ID_ind
)

m_fit <- cstan( file= "models/Returns.stan" , data=dat , chains=3, cores = 3 )
post <- extract.samples(m_fit)

for(i in 1:100){
  phi <- (1-exp(-post$beta_a[i] * seq(0,3,0.1)  )) ^ post$gamma_a[i]
  lines( seq(0,3,0.1) ,  phi, col = col.alpha("lightblue", 0.7))
}


#set list of parameters
s <- list ( c(2, 0.1),
            c(1, 0.5),
            c(0.8, 0.1),
            c(0.1, 3)
            )
labsss <- c( "1", "2", "3", "4")
par(mfrow = c(2,2))

#loop over parameters set
for (i in 1:4) {
    zs <- s[[i]] #assign set of values for simulation
    plot( NULL, xlim = c(0,4), ylim = c(0,4),
          xlab = "simulated effect", ylab = "estimated effect",
          main = labsss[i])
    
    for(j in 1:5){
        d <- sim_data(100, 300, zero = F,
                      b_a = zs[1], g_a = zs[2]
                      )
        dat <- list(
          N = d$N,
          M = d$M,
          A = d$A/mean(d$A),
          R = d$R,
          L = d$L/mean(d$L),
          K = d$K/mean(d$K),
          ID_ind = d$ID_ind
        )

        m <- cstan( file = "models/Returns.stan" , data=dat , 
                    chains=3 , cores = 3 )
        prec <- precis(m)
        points(  zs, prec$mean[2:3], xlim = c(0,1.5), ylim = c(0,1.5), 
                 pch = c (15, 16, 17), col = col.alpha("gray40", 0.8))
        abline(0,1)
    }
}
par(mfrow = c(1,1))


#################

#SUCCESS
#
d<-sim_data(100,300, zero = T, b_a = 0.2, g_a = 0.2)
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A/mean(d$A),
  S = d$S,
  L = d$L/mean(d$L),
  K = d$K/mean(d$K),
  ID_ind = d$ID_ind
)

m_fit <- cstan( file= "models/Success.stan" , data=dat , chains=3, cores = 3 )
post <- extract.samples(m_fit)



###################
#apply to data

d <- list.load("2_data_preparation/processed_data.RData")

d_complete_ppl <- d$shell_ppl[complete.cases(d$shell_ppl$age),]
d_complete_shells <- d$shells[which(d$shells$anonymeID %in% d_complete_ppl$anonymeID),]
dat <- list(
  N = nrow(d_complete_ppl),
  M = nrow(d_complete_shells),
  A = d_complete_ppl$age[order(d_complete_ppl$anonymeID)] / mean(d_complete_ppl$age),
  R = as.numeric(d_complete_shells$returns)/100,
  L = d_complete_shells$lenght_min/mean(d_complete_shells$lenght_min),
  ID_ind= as.integer(as.factor(as.character(d_complete_shells$anonymeID)))
)


m_fit <- cstan( file= "models/Returns.stan" , data=dat , chains=3, cores = 3 )
precis(m_fit)
post <- extract.samples(m_fit)
plot(dat$A[dat$ID_ind], dat$R, xlim = c(0,3),ylim = c (0,100))
for(i in 1:550){
  phi <- (1-exp(-post$beta_a[i] * seq(0,3,0.1)  )) ^ post$gamma_a[i]
  psi<- (mean(dat$L))^post$lambda[i]#this is 1 by default
  R <- exp ( post$alpha[i] + phi + psi + ((post$sigma[i]^2) /2))
  lines( seq(0,3,0.1) ,  R, col = col.alpha("cornflowerblue", 0.7))
}
