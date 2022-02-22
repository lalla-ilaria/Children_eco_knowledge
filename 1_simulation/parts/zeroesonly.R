#PRIOR SIMULATION
x <- seq(0,3, 0.1) #trait
a <- rlnorm(100, 0, 1)#prior for effects of trait
c <- rnorm(100, 1,0.2)#parameter to move center of logit curve - in its absence all curves are forced to pass through x=1,y=0.5 and it distorts the curves. Makes sense?
g <- matrix(nrow = length(a), ncol = length(x))#store
plot(NULL, xlim = c(0,3), ylim = c(0,1), 
     xlab = "trait value", ylab = "prob non zero returns")
for (i in 1:length(a)) {
  g[i,] <- 1 - (inv_logit (  2 * ( c[i] - exp( a[i] * log(x) + a[i] * log(1) ))))# transformed to cover right space
  lines( x, g[i,], col = col.alpha("cornflowerblue", 0.7))
} 


#SIMULATE DATA
source("1_simulation/1_simulation.R")

#simulate data with effects of length of trip (l), knowledge (k) and body (b) of individuals on probability of non zeroes (notz) 
d <- sim_data(100, 300,
              zl = 0.5, zk = 0.5, zb = 0.5)

#review simulated data
plot(d$L, d$p, 
     pch = 16, col = col.alpha("grey40", 0.2), 
     xlab = "length of trip", ylab = "prob non zero returns")
plot(d$K[d$ID_trip], d$p, 
     pch = 16, col = col.alpha("grey40", 0.2), 
     xlab = "knowledge", ylab = "prob non zero returns")
plot(d$B[d$ID_trip], d$p, 
     pch = 16, col = col.alpha("grey40", 0.2), 
     xlab = "body traits", ylab = "prob non zero returns")

plot(d$L, d$notZ, 
     pch = 16, col = col.alpha("grey40", 0.2), 
     xlab = "length of trip", ylab = "non zero returns")
plot(d$K[d$ID_trip], d$notZ, 
     pch = 16, col = col.alpha("grey40", 0.2), 
     xlab = "length of trip", ylab = "non zero returns")
plot(d$B[d$ID_trip], d$notZ, 
     pch = 16, col = col.alpha("grey40", 0.2), 
     xlab = "length of trip", ylab = "non zero returns")

#FIT MODEL
#prepare data
dat <- list(
  M = d$M,
	notZ = d$notZ,
	L = d$L/mean(d$L),
	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

#fit model
m <- cstan( file= "models/zerosonly.stan" , data=dat , chains=1 )
precis(m)

#CHECK MODEL FIT
x <- seq(0, 3, 0.1) #trait

post <- extract.samples(m)

plot(d$L/mean(d$L), d$notZ, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,3))
for (i in 1:500) lines(x, 1 - (inv_logit (  2 * ( post$c[i] - 
                                exp( post$z_l[i] * log(x)  )))) , col = col.alpha("orange", 0.4))

plot(d$K[d$ID_trip]/mean(d$K), d$notZ, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,3))
for (i in 1:500) lines(x, 1 - (inv_logit (  2 * ( post$c[i] - 
                                exp( post$z_k[i] * log(x)  )))) , col = col.alpha("orange", 0.4))

plot(d$B[d$ID_trip]/mean(d$B), d$notZ, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,3))
for (i in 1:500) lines(x, 1 - (inv_logit (  2 * ( post$c[i] - 
                                exp( post$z_b[i] * log(x)  )))) , col = col.alpha("orange", 0.4))

#FIT MODEL MULTIPLE TIMES
#define list of parameter combinations
s <- list ( c(2, 0.1, 3),
            c(2, 0.1, 3),
            c(2, 0.1, 3),
            c(0.8, 0.2, 0.3)
            )
age <- c(T,T,F,F) #whether age has an effect at all on knowledge and body. Tests whether correlation between K and B impairs the model ability to distinguish between the two
a <- c(3, 0.5, 1, 1) #effect of age on knowledge and body
labsss <- c( "big age effect on K & B", "small age effect on K & B", "no age effect on K & B", "no age effect on K & B")
par(mfrow = c(2,2))

for (i in 1:4) {
zs <- s[[i]] #assign set of values for simulation
plot( NULL, xlim = c(0,3.1), ylim = c(0,3.1),
      xlab = "simulated effect", ylab = "estimated effect",
      main = labsss[i])

for(j in 1:5){
d <- sim_data(100, 300, 
              zl = zs[1], zk = zs[2], zb = zs[3],
              ak = a[i], ab = a[i], age = age[i]
              )

dat <- list(
  M = d$M,
	notZ = d$notZ,
	L = d$L/mean(d$L),
	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

m <- cstan( file= "models/zerosonly.stan" , data=dat , chains=1 )

prec <- precis(m)
post <- extract.samples(m)

points( zs, prec$mean[2:4], xlim = c(0,1.5), ylim = c(0,1.5), pch = c (15, 16, 17), col = col.alpha("gray40", 0.8))
abline(0,1)
}
}
