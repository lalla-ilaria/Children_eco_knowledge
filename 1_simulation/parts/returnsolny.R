#PRIOR PREDICTIVE SIMULATION
x <- seq(0,3, 0.1) #trait
a <- rlnorm(100, 0, 1)#prior for effects of trait
g <- matrix(nrow = length(a), ncol = length(x))#store
plot(NULL, xlim = c(0,3), ylim = c (0,10), 
     xlab = "trait value", ylab = "returns amount")
for (i in 1:length(a)) {
  g[i,] <-  x^a[i]   #chaos?!? how to plot lognormals???
  lines( x,  rlnorm(length(x),g[i,],0 ), col = col.alpha("cornflowerblue", 0.7))
}


#SIMULATE DATA
source("1_simulation/1_simulation.R")

#simulate data with effects of length of trip (l), knowledge (k) and body (b) of individuals on probability of non zeroes (notz) and returns (r)
d <- sim_data(100, 300, zero = F, age = F, #simulating without zeroes, without effect of age
              rl = 1, rk = 0.2, rb = 3)

#review simulated data
plot(d$L, d$R, 
     pch = 16, col = col.alpha("grey40", 0.2), ylim = c(0,8),
     xlab = "length of trip", ylab = "returns amount")
plot(d$K[d$ID_trip], d$R, 
     pch = 16, col = col.alpha("grey40", 0.2),ylim = c(0,8),
     xlab = "knowledge", ylab = "returns amount")
plot(d$B[d$ID_trip], d$R, 
     pch = 16, col = col.alpha("grey40", 0.2), ylim = c(0,8),
     xlab = "body traits", ylab = "returns amount")

#FIT MODEL
#prepare data
dat <- list(
  M = d$M,
  R = d$R,
  L = d$L/mean(d$L),
  K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
  B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

#fit model
m <- cstan( file= "models/returnsonly.stan" , data=dat , chains=1 )
precis(m)
tracerplot(m) #not exploring much the space?

#CHECK MODEL FIT
x <- seq(0, 3, 0.1) #trait

post <- extract.samples(m)

#weird plot. I'm doing something wrong.
#fist, simulated data from lognormal include only numbers above zero
#second, I need to transform the result of the model in the log scale or something???
#but it fits something
plot(d$L/mean(d$L), d$R, pch = 16, col = col.alpha("grey30", 0.2), ylim=c(0,10))
for (i in 1:500) lines(x, exp( post$r_l[i] * log(x) + post$r_k[i] * log(mean(dat$K)) + post$r_b[i] * log(mean(dat$B))), col = col.alpha("orange", 0.4))

plot(dat$K, d$R, pch = 16, col = col.alpha("grey30", 0.2), ylim=c(0,10))
for (i in 1:500) lines(x, exp( post$r_l[i] * log(mean(dat$L)) + post$r_k[i] * log(x) + post$r_b[i] * log(mean(dat$B))), col = col.alpha("orange", 0.4))

plot(dat$B, d$R, pch = 16, col = col.alpha("grey30", 0.2), ylim=c(0,10))
for (i in 1:500) lines(x, exp( post$r_l[i] * log(mean(dat$L)) + post$r_k[i] * log(mean(dat$K)) + post$r_b[i] * log(x)), col = col.alpha("orange", 0.4))


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
