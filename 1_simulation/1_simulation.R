sim_data <- function (N , M ,    #number of 
         shell = F, #skill as a function of knowledge and body? Or one single cobb douglas with K and B determining returns? 
         #individual effects
         ak = 0.1,  #effect of age on knowledge
         ab = 0.2,  #effect of age on body
         #trip zero returns effect
         zl = 0.2 , #effect of time spent foraging
         zk = 0.3 , #effect of knowledge of child
         zb = 0.4 , #effect of knowledge of child
         #trip amount returns effect
         rl = 0.2 , #effect of time spent foraging
         rk = 0.3 , #effect of knowledge of child
         rb = 0.4   #effect of knowledge of child
){
  ###################
  #general properties
  ###################
  #simulate individual properties
  A <- sample ( 5 : 20, N, replace = T) #age
  K <- 1 - exp( - rnorm (N, ak, 0.01) * A ) #knowledge
  B <- 1 - exp( - rnorm (N, ab, 0.05) * A ) #body

  #simulate trip properties
  L <- abs(rnorm(M, 150, 100)) #labor, duration of foraging trip (min)
  ID_trip <- sample (1:N, size = M, replace = T)#assign trip to individual
  
   ####################
  #returns depend directly from K and B 
  ####################
      p <-  1 - exp( -zl * (L/mean(L)) 
                     -zk * (K[ID_trip]/mean(K[ID_trip]))
                     -zb * (B[ID_trip]/mean(B[ID_trip]))) #probability success per trip given individual traits 
     ifelse ( shell == F, 
               Z <- rbern( M, p = p ), #success of trip 0/1
               Z <- 1
               )#is shellfish?
      
      ##calculate foraging returns
      R <- Z * rlnorm( M , log( L ^ (rl)  * 
                                  K[ID_trip] ^ (rk) * 
                                  B[ID_trip] ^ (rb) ) , 0.5 )  #FORAGING RETURNS
  return( list (N = N, #number of kids
                M = M, #number of trips
                A = A, #ages
                K = K, #knowledge
                B = B, #body
                L = L, #duration  of trips
                ID_trip = ID_trip, #child per trip
                Z = Z, #zero returns. All 1 if shell = T
                R = R  #returns amount
                ) )
}#simulation
