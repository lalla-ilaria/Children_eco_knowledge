#load packages and simulation function
library(rethinking)
source("1_simulation/1_simulation.R")

#simulate data with effects of lenght of trip (l), knowledge (k) and body (b) of individuals on probability of non zeroes (z) and returns (r)
d <- sim_data(100, 300, 
              zl = 0.3, zk = 0.8, zb = 0.2,
              rl = 0.3, rk = 0.8, rb = 0.2
              )

#prepare data
dat <- list(
  M = d$M,
	Z = d$Z,
	R = d$R,
	L = d$L/mean(d$L),
	K = d$K[d$ID_trip]/mean(d$K[d$ID_trip]),
	B = d$B[d$ID_trip]/mean(d$B[d$ID_trip])
)

#fit model
m <- cstan( file= "models/model.stan" , data=dat , chains=1 )
