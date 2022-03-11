#PRIOR SIMULATION
x <- seq(0,3, 0.1) #trait
a <- rlnorm(100, 0, 1)#prior for effects of trait
c <- abs(rnorm(100, 0,1))#parameter to move center of logit curve - in its absence all curves are forced to pass through x=1,y=0.5 and it distorts the curves. Makes sense?
g <- matrix(nrow = length(a), ncol = length(x))#store
plot(NULL, xlim = c(0,3), ylim = c(0,3), 
     xlab = "trait value", ylab = "theta")
for (i in 1:length(a)) {
  g[i,] <-   1 - 2 * ( 1 - inv_logit(  c[i] * x^a[i]))# transformed to cover right space
  lines( x, g[i,], col = col.alpha("cornflowerblue", 0.7))
} 
#1 - 2 * ( 1 - inv_logit(   alpha *  K[i] ^ z_k *  ))
#curve( 1 - 2*( 1 - inv_logit( 0.5*x^0.5 ) ) , from=0 , to=10 , ylim=c(0,1) )

#SIMULATE DATA
source("1_simulation/1_simulation.R")

#simulate data with effects of length of trip (l), knowledge (k) and body (b) of individuals on probability of non zeroes (notz) 
sd <- sim_data(100, 300,
              zl = 0.2, zk = 0.5, zb = 0.2)

#review simulated data
plot(jitter(sd$L), sd$S, 
     pch = 16, col = col.alpha("grey40", 0.2), 
     xlab = "length of trip", ylab = "non zero returns")
plot(sd$K[sd$ID_trip], sd$S, 
     pch = 16, col = col.alpha("grey40", 0.2), 
     xlab = "knowledge", ylab = "non zero returns")
plot(sd$B[sd$ID_trip], sd$S, 
     pch = 16, col = col.alpha("grey40", 0.2), 
     xlab = "body", ylab = "non zero returns")

#FIT MODEL
#prepare data
dat <- list(
  M = sd$M,
	S = sd$S,
	L = sd$L,
	K = sd$K[sd$ID_trip]/mean(sd$K[sd$ID_trip]),
	B = sd$B[sd$ID_trip]/mean(sd$B[sd$ID_trip])
)

#FIT WITH DATA
d <- list.load("2_data_preparation/processed_data.RData")
d$traps$lenghtDay <-ifelse(d$traps$lenghtDay==0, 0.01, d$traps$lenghtDay ) 
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
m <- cstan( file= "models/zerosonly.stan" , data=dat , chains=1 )
precis(m)
tracerplot(m)
dev.off()

#CHECK MODEL FIT
x <- seq(0, 7, 0.1) #trait

post <- extract.samples(m)

#simulate from posterior BINOMIAL
n_days <- 10
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


#plot posterior predictions
plot(dat$L, dat$S, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,5))
for (i in 1:500) lines(x, post$alpha[i] * x ^ post$z_l[i]  , col = col.alpha("orange", 0.4))

plot(dat$K, dat$S, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,3))
for (i in 1:500) lines(x, post$alpha[i] * x ^ post$z_k[i]  , col = col.alpha("orange", 0.4))

plot(dat$B, dat$S, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,3))
for (i in 1:500) lines(x, post$alpha[i] * x ^ post$z_b[i]  , col = col.alpha("orange", 0.4))


plot(dat$L, dat$S, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,7))
for (i in 1:500) lines(x, mean(dat$L) * 1 - 2*(1-inv_logit  (
  post$alpha[i] * x ^ post$z_l[i] )) , col = col.alpha("orange", 0.4))


plot(dat$L, dat$S, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,7))
for (i in 1:500) lines(x, 1 - 2*(1-inv_logit  (
  post$alpha[i] * x ^ post$z_l[i] )) , col = col.alpha("orange", 0.4))

plot(dat$K, dat$S, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,7))
for (i in 1:500) lines(x, 1 - 2*(1-inv_logit  (
                                 post$alpha[i] * x ^ post$z_k[i] )) , col = col.alpha("orange", 0.4))

plot(dat$B, dat$S, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0,3))
for (i in 1:500) lines(x, 1 - 2*(1-inv_logit  (
                                 post$alpha[i] * x ^ post$z_b[i] )) , col = col.alpha("orange", 0.4))

#FIT MODEL MULTIPLE TIMES
#define list of parameter combinations
s <- list ( c(0.8, 0.5, 0.1),
            c(0.8, 0.5, 0.1),
            c(0.8, 0.1, 0.3),
            c(0.8, 0.1, 0.3)
            )
zalp <- c( 0.1, 0.5, 0.1, 0.5)
age <- c(T,T,F,F) #whether age has an effect at all on knowledge and body. Tests whether correlation between K and B impairs the model ability to distinguish between the two
a <- c(1, 1, 1, 1) #effect of age on knowledge and body
labsss <- c( "1", "2", "3", "4")
par(mfrow = c(2,2))

for (i in 1:4) {
zs <- s[[i]] #assign set of values for simulation
plot( NULL, xlim = c(0,1.1), ylim = c(0,1.1),
      xlab = "simulated effect", ylab = "estimated effect",
      main = labsss[i])

for(j in 1:5){
d <- sim_data(100, 500, 
              zl = zs[1], zk = zs[2], zb = zs[3],
              ak = a[i], ab = a[i], age = age[i], zalpha = zalp[i]
              )
print(table(d$S))

dat <- list(
  M = d$M,
	S = d$S,
	L = d$L,
	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

m <- cstan( file= "models/zerosonlyBinom.stan" , data=dat , chains=1 )

prec <- precis(m)
#print(prec$mean[1])
post <- extract.samples(m)

points( c( zalp[i], zs[2:3]), prec$mean[1:3], xlim = c(0,1.5), ylim = c(0,1.5), pch = c (15, 16, 17), col = col.alpha("gray40", 0.8))
abline(0,1)
}
}
