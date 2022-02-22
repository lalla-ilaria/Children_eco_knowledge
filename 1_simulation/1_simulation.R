sim_data <- function (N , M ,    #number of 
         zero = T, #having zero results - or not, as in the case of shellfish
         age = T, #are body and knowledge linked to age?
         #individual effects
         ak = 1,  #effect of age on knowledge
         ab = 1,  #effect of age on body
         #trip zero returns effect
         zl = 0.3 , #effect of time spent foraging
         zk = 0.3 , #effect of knowledge of child
         zb = 0.3 , #effect of knowledge of child
         #trip amount returns effect
         rl = 0.3 , #effect of time spent foraging
         rk = 0.3 , #effect of knowledge of child
         rb = 0.3   #effect of knowledge of child
){
  ###################
  #general properties
  ###################
  #simulate individual properties
  A <- sample ( 5 : 20, N, replace = T) #age
  
  K <- abs( rnorm (N, ak, 0.3 * ak) * if( age == T) A else runif(N, 3, 13))  #knowledge
  B <- abs( rnorm (N, ab, 0.3 * ab) * if( age == T) A else runif(N, 3, 13))  #body

  #simulate trip properties
  L <- abs(rnorm(M, 150, 100)) #labor, duration of foraging trip (min)
  ID_trip <- sample (1:N, size = M, replace = T)#assign trip to individual
  
  ####################
  #create outcome 
  ####################
      p <- 1 - inv_logit( 2 * ( 1 - exp(  zl * log( L/mean(L) ) + 
                                          zk * log( K[ID_trip]/mean(K) ) + 
                                          zb * log( B[ID_trip]/mean(B) ))))
      ifelse ( zero == T, #does it inflate zeroes?
               notZ <- rbern( M, p = p ), #success of trip 0/1
               notZ <- 1
               )
      
      ##calculate foraging returns
      R <- notZ * rlnorm( M ,  (L/mean(L)) ^ (rl)  * 
                            (K[ID_trip]/mean(K)) ^ (rk) * 
                            (B[ID_trip]/mean(B)) ^ (rb) , 0.1 )  #FORAGING RETURNS
  return( list (N = N, #number of kids
                M = M, #number of trips
                A = A, #ages
                K = K, #knowledge
                B = B, #body
                L = L, #duration  of trips
                ID_trip = ID_trip, #child per trip
                p = p, #prob success, for testing
                notZ = notZ, #zero returns. All 1 if zero = F
                R = R  #returns amount
                ) )
}#simulation
