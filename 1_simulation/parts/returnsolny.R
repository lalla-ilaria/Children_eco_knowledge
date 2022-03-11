#PRIOR PREDICTIVE SIMULATION
x <- seq(0,3, 0.1) #trait
a <- rlnorm(100, 0, 1)#prior for effects of trait
s <- rexp(100, 1)
g <- matrix(nrow = length(a), ncol = length(x))#store
plot(NULL, xlim = c(0,3), ylim = c (0,10), 
     xlab = "trait value", ylab = "returns amount")
for (i in 1:length(a)) {
  g[i,] <- exp ( x^a[i] + ((s[i]^2) /2))
  lines( x,  g[i,], col = col.alpha("cornflowerblue", 0.7))
}


#SIMULATE DATA
source("1_simulation/1_simulation.R")

#simulate data with effects of length of trip (l), knowledge (k) and body (b) of individuals on probability of non zeroes (notz) and returns (r)
d <- sim_data(100, 300, zero = F, age = T, #simulating without zeroes, with effect of age
              ak = 1, ab = 1,
              rl = 0.4, rk = 0.4, rb = 0.4)
plot(d$A, d$K)
plot(d$A, d$B)

#review simulated data
plot(d$L, d$R, 
     pch = 16, col = col.alpha("grey40", 0.2), ylim = c(0,18),
     xlab = "length of trip", ylab = "returns amount")
plot(d$K[d$ID_trip], d$R, 
     pch = 16, col = col.alpha("grey40", 0.2),ylim = c(0,18),
     xlab = "knowledge", ylab = "returns amount")
plot(d$B[d$ID_trip], d$R, 
     pch = 16, col = col.alpha("grey40", 0.2), ylim = c(0,18),
     xlab = "body traits", ylab = "returns amount")
plot(d$A[d$ID_trip], d$R, 
     pch = 16, col = col.alpha("grey40", 0.2), ylim = c(0,18),
     xlab = "age", ylab = "returns amount")

#FIT MODEL
#prepare data
dat <- list(
  M = d$M,
  A = d$A[d$ID_trip]/mean(d$A[d$ID_trip]),
  R = d$R,
  L = d$L/mean(d$L),
  K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
  B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

#FIT WITH DATA
d <- list.load("2_data_preparation/processed_data.RData")
dat <- list(
  M = nrow(d$shells),
  A = d$shells$age / mean(d$shells$age),
  R = d$shells$returns,
  L = d$shells$lenghtMin/mean(d$shells$lenghtMin),
  K = d$shells$knowledge/mean(d$shells$knowledge),
  B = d$shells$height/mean(d$shells$height)
)

plot(dat$L, dat$R, 
     pch = 16, col = col.alpha("grey40", 0.2),
     xlab = "length of trip", ylab = "returns amount")
plot(dat$K, dat$R, 
     pch = 16, col = col.alpha("grey40", 0.2),
     xlab = "knowledge", ylab = "returns amount")
plot(dat$B, dat$R, 
     pch = 16, col = col.alpha("grey40", 0.2),
     xlab = "body traits", ylab = "returns amount")
plot(dat$A, dat$R, 
     pch = 16, col = col.alpha("grey40", 0.2),
     xlab = "body traits", ylab = "returns amount")


#fit model
m <- cstan( file= "models/returnsonly.stan" , data=dat , chains=1 )
precis(m)
tracerplot(m) #not exploring much the space?
par(mfrow = c(1,1))
dev.off()
#CHECK MODEL FIT
x <- seq(0, 3, 0.1) #trait

post <- extract.samples(m)

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
  mu <- exp ( exp( post$alpha[i] + post$a_a[i] * log(x)) + ((post$sigma[i]^2) /2))
  lines(x, mu, col = col.alpha("orange", 0.4))
}

plot(dat$A, dat$K, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0, 3))
for (i in 1:500) {
  mu <-  post$alpha_k[i] + x ^ post$k_a[i]
  lines(x, mu, col = col.alpha("orange", 0.4))
}
plot(dat$A, dat$B, pch = 16, col = col.alpha("grey30", 0.2), xlim = c(0, 3))
for (i in 1:500) {
  mu <-  post$alpha_b[i] + x ^ post$b_a[i]
  lines(x, mu, col = col.alpha("orange", 0.4))
}


#FIT MODEL MULTIPLE TIMES
#define list of parameter combinations
s <- list ( c(2, 0.1, 3),
             c(2, 0.5, 0.9),
             c(2, 0.1, 3),
             c(0.8, 0.2, 0.3)
             )
age <- c(T,T,F,F) #whether age has an effect at all on knowledge and body
a <- c(3, 0.5, 1, 1) #effect of age on knowledge and body
labsss <- c( "big age effect on K & B", "small age effect on K & B", "no age effect on K & B", "no age effect on K & B")
par(mfrow = c(2,2))
 
for (i in 1:4) {
  rs <- s[[i]] #assign set of values for simulation
  plot( NULL, xlim = c(0,3.1), ylim = c(0,3.1),
        xlab = "simulated effect", ylab = "estimated effect",
         main = labsss[i])
   
   for(j in 1:5){
       d <- sim_data(100, 300, zero = F,
                     rl = rs[1], rk = rs[2], rb = rs[3],
                     ak = a[i], ab = a[i], age = age[i]
                     )
       
       dat <- list(
          M = d$M,
         	R = d$R,
         	L = d$L/mean(d$L),
         	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
          B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
      )
      
      m <- cstan( file= "models/returnsonly.stan" , data=dat , chains=1 )
      
      prec <- precis(m)
      post <- extract.samples(m)
      
      points( rs, prec$mean[1:3], pch = c (15, 16, 17), col = col.alpha("gray40", 0.8))
      abline(0,1)
  }
}
par(mfrow = c(1,1))



data(WaffleDivorce) 
d <- list()
d$A <- standardize( WaffleDivorce$MedianAgeMarriage )
d$D <- standardize( WaffleDivorce$Divorce )
d$M <- standardize( WaffleDivorce$Marriage )
m5.3_A <- ulam(
alist(
## A -> D <- M
D ~ dnorm( mu , sigma ) ,
mu <- a + bM*M + bA*A ,
a ~ dnorm( 0 , 0.2 ) ,
bM ~ dnorm( 0 , 0.5 ) ,
bA ~ dnorm( 0 , 0.5 ) ,
sigma ~ dexp( 1 ),
## A -> M
M ~ dnorm( mu_M , sigma_M ),
mu_M <- aM + bAM*A,
aM ~ dnorm( 0 , 0.2 ),
bAM ~ dnorm( 0 , 0.5 ),
sigma_M ~ dexp( 1 )
) , data = d )
