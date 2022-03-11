#load packages and simulation function
library(rethinking)
library(rlist)
source("1_simulation/1_simulation.R")

#simulate data with effects of length of trip (l), knowledge (k) and body (b) of individuals on probability of non zeroes (z) and returns (r)
d <- sim_data(100, 300)#all parameters are 0.3
# d <- sim_data(100, 300, #example with different parameters
#               zl = 0.3, zk = 0.8, zb = 0.2,
#               rl = 0.3, rk = 0.8, rb = 0.2
#               )


#prepare data
dat <- list(
  M = d$M,
	notZ = d$notZ,
	R = d$R,
	L = d$L/mean(d$L),
	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

#fit model
m <- cstan( file= "models/model.stan" , data=dat , chains=1 )
precis(m) #looks good!
tracerplot(m) # parameters for returns not exploring much the space?
par(mfrow = c(1,1))

#CHECK MODEL FIT
#below my attempts at plotting the results of the model
#not zero seems fine, amount of results I'm doing something wrong
################
x <- seq(0, 3, 0.1) #trait
post <- extract.samples(m)

#Not zero
plot(d$L/mean(d$L), d$notZ, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,3))
for (i in 1:500) lines(x, 1 - (inv_logit (  2 * ( post$c[i] - 
                                exp( post$z_l[i] * log(x)  )))) , col = col.alpha("orange", 0.4))

plot(d$K[d$ID_trip]/mean(d$K), d$notZ, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,3))
for (i in 1:500) lines(x, 1 - (inv_logit (  2 * ( post$c[i] - 
                                exp( post$z_k[i] * log(x)  )))) , col = col.alpha("orange", 0.4))

plot(d$B[d$ID_trip]/mean(d$B), d$notZ, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,3))
for (i in 1:500) lines(x, 1 - (inv_logit (  2 * ( post$c[i] - 
                                exp( post$z_b[i] * log(x)  )))) , col = col.alpha("orange", 0.4))
#Amount of results
plot(d$L/mean(d$L), d$R, pch = 16, col = col.alpha("grey30", 0.2), ylim=c(0,10))
for (i in 1:500) {
  mu <- exp ( exp( post$r_l[i] * log(x)) + ((post$sigma[i]^2) /2))
  lines(x, mu, col = col.alpha("orange", 0.4))
}

plot(dat$K, d$R, pch = 16, col = col.alpha("grey30", 0.2), ylim=c(0,10))
for (i in 1:500) {
  mu <- exp ( exp( post$r_k[i] * log(x)) + ((post$sigma[i]^2) /2))
  lines(x, mu, col = col.alpha("orange", 0.4))
}

plot(dat$B, d$R, pch = 16, col = col.alpha("grey30", 0.2), ylim=c(0,10))
for (i in 1:500) {
  mu <- exp ( exp( post$r_b[i] * log(x)) + ((post$sigma[i]^2) /2))
  lines(x, mu, col = col.alpha("orange", 0.4))
}
################

#FIT MODEL MULTIPLE TIMES
#plots recovered parameters versus estimated parameters 
#in gray the parameters for non zero returns, orange for amount of returns
#seems to be doing a good job of recovering the parameters!
###############
#define list of parameter combinations
s <- list ( c(2, 0.1, 3),
             c(2, 0.5, 0.9),
             c(2, 0.1, 3),
             c(0.8, 0.2, 0.3)
             )
age <- c(T,T,F,F)  #whether age has an effect at all on knowledge and body. Tests whether correlation between K and B impairs the model ability to distinguish between the two
a <- c(3, 0.5, 1, 1) #effect of age on knowledge and body
labsss <- c( "big age effect on K & B", "small age effect on K & B", "no age effect on K & B", "no age effect on K & B")
par(mfrow = c(2,2))

#run 5 models for each of the 4 combinations of parameters
for (i in 1:4) {
  zs <- s[[i]]
  rs <- s[[i]] #assign set of values for simulation
  plot( NULL, xlim = c(0,3.1), ylim = c(0,3.1),
       xlab = "simulated effect", ylab = "estimated effect",
       main = labsss[i])

  for(j in 1:5){
    d <- sim_data(100, 300, 
              zl = zs[1], zk = zs[2], zb = zs[3],
              rl = rs[1], rk = rs[2], rb = rs[3],
              ak = 0.5, ab = 0.5, age = a[i]
              )

    dat <- list(
              M = d$M,
            	notZ = d$notZ,
            	R = d$R,
            	L = d$L/mean(d$L),
            	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
            	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
      )

      m <- cstan( file= "models/model.stan" , data=dat , chains=1 )

      prec <- precis(m)
      post <- extract.samples(m)

      points( zs-0.01, prec$mean[2:4], xlim = c(0,1.5), ylim = c(0,1.5), pch = c (15, 16, 17), col = col.alpha("gray40", 0.8))
      points( rs+0.01, prec$mean[5:7], xlim = c(0,1.5), ylim = c(0,1.5), pch = c (15, 16, 17), col = col.alpha("orange", 0.8))
      abline(0,1)
    }
}

par(mfrow = c(1,1))
###############


