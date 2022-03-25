#################
#load packages, simulation function and data
library(rethinking)
library(rlist)
source("1_simulation/1_simulation.R")
real_data <- list.load("2_data_preparation/processed_data.RData")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
########
###TRAPS
########
#Traps are remounted after they capture something, so they can have more 
#than one success. For this reason we cannot use a Bernoulli distribution. 
#I tried instead with both a Poisson and Binomial distribution. 

#POISSON------------------------------------------------------------------------
#########
#First Poisson. 
#It models the number of events that happen in a fixed time or space at a 
#certain rate lambda. This is not an accurate representation of our case, 
#as the time is not fixed, on the contrary, traps can remain mounted anywhere 
#from few days to over a month. Still, it works.
###########

#simulate poisson distributed data
d <- sim_data(100, 300, distribution = "poisson",
              zl = 0.2, zk = 0.5, zb = 0.2)
dat <- list(
  M = d$M,
	S = d$S,
	L = d$L/mean(d$L),
	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

m <- cstan( file = "models/zerosonlyPoiss.stan" , data=dat , chains=3, cores = 3 )
precis(m)

#########
#Testing poisson model repeated times.
#In the final plot, the points should fall on or parallel to the diagonal line.
#Square dots represent the effect of time, diamond dots are the effect of knowledge, 
#and triangles the effect of body, as simulated. 
#It does a reasonable job as long as the effects are sufficiently different, 
#i.e. as long as the alpha parameter is big enough. With alpha = 0.1 it has some 
#problem distinguishing similar effects of body and knowledge.
########

#set list of parameters
s <- list ( c(0.8, 0.5, 0.1),
            c(0.8, 0.5, 0.1),
            c(0.8, 0.1, 0.3),
            c(0.8, 0.1, 0.3)
            )
zalp <- c( 0.1, 0.5, 0.1, 0.5)
age <- c(T,T,F,F) #whether age has an effect at all on knowledge and body. Tests whether correlation between K and B impairs the model ability to distinguish between the two
a <- c(1, 1, 1, 1) #effect of age on knowledge and body
labsss <- c( "alpha=0.1, age=T", 
             "alpha=0.5, age=T", 
             "alpha=0.1, age=F", 
             "alpha=0.5, age=F")
par(mfrow = c(2,2))

#loop over parameters set
for (i in 1:4) {
    zs <- s[[i]] #assign set of values for simulation
    plot( NULL, xlim = c(0,1.1), ylim = c(0,1.1),
          xlab = "simulated effect", ylab = "estimated effect",
          main = labsss[i])
    
    for(j in 1:5){
        d <- sim_data(100, 300, distribution = "poisson",
                      zl = zs[1], zk = zs[2], zb = zs[3],
                      ak = a[i], ab = a[i], age = age[i], zalpha = zalp[i]
                      )
        dat <- list(
          M = d$M,
        	S = d$S,
        	L = d$L/mean(d$L),
        	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
        	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
        )
        m <- cstan( file = "models/zerosonlyPoiss.stan" , data=dat , 
                    chains=3 , cores = 3 )
        prec <- precis(m)
        points(  zs, prec$mean[2:4], xlim = c(0,1.5), ylim = c(0,1.5), 
                 pch = c (15, 16, 17), col = col.alpha("gray40", 0.8))
        abline(0,1)
    }
}
par(mfrow = c(1,1))


##############
#Trying on real data. 
##############
#FIT WITH DATA
d <- real_data
d$traps$lenghtDay <-ifelse(d$traps$lenghtDay==0, 0.01, d$traps$lenghtDay ) 
dat <- list(
  M = nrow(d$traps),
  S = d$traps$success,
  L = d$traps$lenghtDay/mean(d$traps$lenghtDay),
  K = d$traps$knowledge/109.8571,
  B = d$traps$height/148.80093
)
#mean values 
# height    weight      grip      knowledge
# 148.80093  39.31875  30.91204   109.8571

#fit model
m <- cstan( file= "models/zerosonlyPoiss.stan" , data=dat , chains=1 )
precis(m)

#CHECK MODEL FIT
x <- seq(0, 5, 0.1) #trait

post <- extract.samples(m)

#simulate from posterior poisson
#length of trap
plot(NULL, xlab = "days", ylab = "n success", xlim = c(0,5), ylim = c (0,4))
for(i in 1:500){
  success <- rpois (length(x), post$alpha[i] * x ^ post$z_l[i] )
  points(jitter(x), jitter(success), pch = 16 , col = col.alpha("grey40", 0.1))
}
points(dat$L, dat$S, pch = 16, col = col.alpha("orange", 0.5))

#knowledge
plot(NULL, xlab = "knowledge", ylab = "n success", xlim = c(0,3), ylim = c (0,4))
for(i in 1:500){
  success <- rpois (length(x), post$alpha[i] * x ^ post$z_k[i] )
  points(jitter(x), jitter(success), pch = 16 , col = col.alpha("grey40", 0.1))
}
points(dat$K, dat$S, pch = 16, col = col.alpha("orange", 0.5))

#body
plot(NULL, xlab = "days", ylab = "n success", xlim = c(0,3), ylim = c (0,4))
for(i in 1:500){
  success <- rpois (length(x), post$alpha[i] * x ^ post$z_b[i] )
  points(jitter(x), jitter(success), pch = 16 , col = col.alpha("grey40", 0.1))
}
points(dat$B, dat$S, pch = 16, col = col.alpha("orange", 0.5))



###################
#BINOMIAL------------------------------------------------------------------------
###################
#Because I think it better describes the data, I went on to try a binomial model. 
#Binomials model the number of events over N trials, with probability theta of 
#happening in each trial. In our case, each day a trap was mounted represents a trial, 
#and the probability of each event theta depends only on body and knowledge (up to now).
################
#simulate data
d <- sim_data(100, 300, distribution = "binomial",
              zl = 0.2, zk = 0.5, zb = 0.2)
dat <- list(
  M = d$M,
	S = d$S,
	L = d$L,
	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

m <- cstan( model_code = binomial_model , data=dat , chains=1 )
print("zl = 0.2, zk = 0.5, zb = 0.2")
precis(m)

########
#Testing binomial model repeated times.
#In the final plot, the points should fall on or parallel to the diagonal line.
#Square dots represent the intercept alpha, diamond dots are the effect of knowledge, 
#and triangles the effect of body, as simulated. 
#Similarly to the poisson model, if the effects are small (i.e. small alpha), the model 
#has difficulties in distinguishing between similar parameters for body and knowledge. 
######

s <- list ( c(0.8, 0.5, 0.2),
            c(0.8, 0.5, 0.2),
            c(0.8, 0.2, 0.4),
            c(0.8, 0.2, 0.4)
            )
zalp <- c( 0.1, 0.6, 0.1, 0.6)
age <- c(T,F,F,T) #whether age has an effect at all on knowledge and body. Tests whether correlation between K and B impairs the model ability to distinguish between the two
a <- c(1, 1, 1, 1) #effect of age on knowledge and body
labsss <- c( "alpha=0.1, age=T", 
             "alpha=0.5, age=F", 
             "alpha=0.1, age=T", 
             "alpha=0.5, age=F")
par(mfrow = c(2,2))

for (i in 1:4) {
    zs <- s[[i]] #assign set of values for simulation
    plot( NULL, xlim = c(0,1.1), ylim = c(0,1.1),
          xlab = "simulated effect", ylab = "estimated effect",
          main = labsss[i])
    
    for(j in 1:5){
        d <- sim_data(100, 300, distribution = "binomial",
                      zl = zs[1], zk = zs[2], zb = zs[3],
                      ak = a[i], ab = a[i], age = age[i], zalpha = zalp[i]
                      )
        dat <- list(
          M = d$M,
        	S = d$S,
        	L = d$L,
        	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
        	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
        )
        m <- cstan(  file = "models/zerosonlyBinom.stan" , data=dat , 
                     chains=3 , cores = 3 )
        prec <- precis(m)
        points(  c( zalp[i], zs[2:3]), prec$mean[1:3], xlim = c(0,1.5), ylim = c(0,1.5), 
                 pch = c (15, 16, 17), col = col.alpha("gray40", 0.8))
        abline(0,1)
    }
}
par(mfrow = c(1,1))

##############
#Trying on real data. 
##############
#FIT WITH DATA
d <- real_data
d$traps$lenghtDay <-round(d$traps$lenghtDay) 
dat <- list(
  M = nrow(d$traps),
  S = d$traps$success,
  L = d$traps$lenghtDay,
  K = d$traps$knowledge/109.8571,
  B = d$traps$height/148.80093
)
#mean values 
# height    weight      grip      knowledge
# 148.80093  39.31875  30.91204   109.8571

#fit model
m <- cstan( file= "models/zerosonlyBinom.stan" , data=dat , chains=1 )
precis(m)

#CHECK MODEL FIT
x <- seq(0, 7, 0.1) #trait

post <- extract.samples(m)

#simulate from posterior binomial 
n_days <- 5 #can change day of exposure
plot(NULL, xlab = "knowledge", ylab = "n success", xlim = c(0,3), ylim = c (0,4))
for(i in 1:500){
  success <- rbinom (length(x), n_days, 1 - 2*(1-inv_logit  (
  post$alpha[i] * x ^ post$z_k[i] )))
  points(jitter(x), jitter(success), pch = 16 , col = col.alpha("grey40", 0.1))
}
points(dat$K, dat$S, pch = 16, col = col.alpha("orange", 0.5))

plot(NULL, xlab = "body", ylab = "n success", xlim = c(0,3), ylim = c (0,4))
for(i in 1:500){
  success <- rbinom (length(x), n_days, 1 - 2*(1-inv_logit  (
    post$alpha[i] * x ^ post$z_b[i] )))
  points(jitter(x), jitter(success), pch = 16 , col = col.alpha("grey40", 0.1))
}
points(dat$B, dat$S, pch = 16, col = col.alpha("orange", 0.5))

#plot depending on day exposure
plot(NULL, xlab = "days", ylab = "n success", xlim = c(0,35), ylim = c (0,4))
for(i in 1:500){
  success <- rbinom (length(1:35), 1:35, 1 - 2*(1-inv_logit  (
    #post$alpha[i]  )))
    post$alpha[i]  * max(dat$K) ^ post$z_k[i] * max(dat$B) ^ post$z_b[i] )))
  points(jitter(1:35), jitter(success), pch = 16 , col = col.alpha("grey40", 0.1))
}
points(dat$L, dat$S, pch = 16, col = col.alpha("orange", 0.8))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#####################
#RANDOM EFFECTS FOR INDIVIDUALS
#added random effects for individuals
#needs to fiddle a little, doesn't sample very well
#ok, added pooling and it fits so much better
###################
d <- sim_data(100, 300, zero = F, age = T, #simulating without zeroes, with effect of age
              ak = 1, ab = 1,
              rl = 0.4, rk = 0.4, rb = 0.4)
#FIT MODEL
#with simulated data
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A[d$ID_trip]/mean(d$A[d$ID_trip]),
  R = d$R,
  L = d$L/mean(d$L),
  K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
  B = d$B[d$ID_trip]/mean(d$B[d$ID_trip]),
  ID = d$ID_trip
)
#OR
#with real data
d <- list.load("2_data_preparation/processed_data.RData")
dat <- list(
  N = length(unique(d$shells$anonymeID)),
  M = nrow(d$shells),
  A = d$shells$age / mean(d$shells$age),
  R = d$shells$returns,
  L = d$shells$lenghtMin/mean(d$shells$lenghtMin),
  K = d$shells$knowledge/mean(d$shells$knowledge),
  B = d$shells$height/mean(d$shells$height),
  ID= as.integer(as.factor(as.character(d$shells$anonymeID)))
)

#fit model
m <- cstan( file= "models/returns_individualeffects.stan" , data=dat , chains=3, cores = 3 )
precis(m, 2)
tracerplot(m) #not exploring much the space? 
dev.off()

#######################
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
########
###STRATIFY BY AGE
#using the model for returns only
########
#1) age as one of the elements in the Cobb Douglas
#The effect of age is null as long as both knowledge and body are in the equation
#Age is mediated by both body and knowledge in the simulation (interesting to see 
#causal structures at work - I tried to simulate and model different dags and it 
#was cool to see the model results)
############

d <- sim_data(100, 300, zero = F, age = T, #simulating without zeroes, with effect of age
              ak = 1, ab = 1,
              rl = 0.4, rk = 0.4, rb = 0.4)
#FIT MODEL
#with simulated data 
dat <- list(
  M = d$M,
  A = d$A[d$ID_trip]/mean(d$A[d$ID_trip]),
  R = d$R,
  L = d$L/mean(d$L),
  K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
  B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

#OR
#with real data
d <- list.load("2_data_preparation/processed_data.RData")
dat <- list(
  M = nrow(d$shells),
  A = d$shells$age / mean(d$shells$age),
  R = d$shells$returns,
  L = d$shells$lenghtMin/mean(d$shells$lenghtMin),
  K = d$shells$knowledge/mean(d$shells$knowledge),
  B = d$shells$height/mean(d$shells$height)
)



#fit model
m <- cstan( file= "models/returns_age.stan" , data=dat , chains=3, cores = 3 )
precis(m, 2)
tracerplot(m) #not exploring much the space? 
dev.off()



#CHECK MODEL FIT
x <- seq(0, 3, 0.1) #trait

post <- extract.samples(m)

#
plot(dat$L, dat$R, pch = 16, col = col.alpha("grey30", 0.2))
for (i in 1:500) {
  mu <- exp ( exp( post$alpha[i] + post$r_l[i] * log(x)) + ((post$sigma[i]^2) /2))
  lines(x, mu, col = col.alpha("orange", 0.4))
}

plot(dat$K, dat$R, pch = 16, col = col.alpha("grey30", 0.2))
for (i in 1:500) {
  mu <- exp ( exp( post$alpha[i] + post$r_k[i] * log(x)) + ((post$sigma[i]^2) /2))
  lines(x, mu, col = col.alpha("orange", 0.4))
}

plot(dat$B, dat$R, pch = 16, col = col.alpha("grey30", 0.2))
for (i in 1:500) {
  mu <- exp ( exp( post$alpha[i] + post$r_b[i] * log(x)) + ((post$sigma[i]^2) /2))
  lines(x, mu, col = col.alpha("orange", 0.4))
}

plot(dat$A, dat$R, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0, 3))
for (i in 1:500) {
  mu <- exp ( exp( post$alpha[i] + post$r_a[i] * log(x)) + ((post$sigma[i]^2) /2))
  lines(x, mu, col = col.alpha("orange", 0.4))
}

plot(dat$A, dat$K, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0, 3))
for (i in 1:500) {
  mu <-  post$alpha_k[i] + x ^ post$k_a[i]
  lines(x, mu, col = col.alpha("orange", 0.4))
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Individual level random effect + age
#this gets messy, and age starts absorbing a lot of effect from body and knowledge
#also, the model fits much worse
#BUT with pooling everything gets fixed :) 
#(not everything, the parameters for the random effect needs to be fiddled with, they don't sample very well)
###################
#prepare data
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A[d$ID_trip]/mean(d$A[d$ID_trip]),
  R = d$R,
  L = d$L/mean(d$L),
  K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
  B = d$B[d$ID_trip]/mean(d$B[d$ID_trip]),
  ID = d$ID_trip
)

#fit model
m <- cstan( file= "models/returns_age_individualeffects.stan" , data=dat , chains=3, cores = 3 )
precis(m, 2)
tracerplot(m) 
dev.off()


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Individual level random effect + age

#2) age in individual effects
dat <- list(
  N = d$N,
  M = d$M,
  A = d$A/mean(d$A),
  R = d$R,
  L = d$L/mean(d$L),
  K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
  B = d$B[d$ID_trip]/mean(d$B[d$ID_trip]),
  ID = d$ID_trip
)

m <- cstan( file= "models/returns_age_individualeffects_1.stan" , data=dat , chains=3, cores = 3 )
precis(m, 2)
