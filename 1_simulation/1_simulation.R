sim_data <- function (N , M ,    #number of 
         zero = T, #having zero results - or not, as in the case of shellfish
         age = T, #are body and knowledge linked to age?
         distribution = "binomial", 
         #individual effects
         ak = 1,  #effect of age on knowledge
         ab = 1,  #effect of age on body
         #trip zero returns effect
         zl = 0.2 , #effect of time spent foraging
         zk = 0.2 , #effect of knowledge of child
         zb = 0.2 , #effect of knowledge of child
         #trip amount returns effect
         rl = 0.3 , #effect of time spent foraging
         rk = 0.3 , #effect of knowledge of child
         rb = 0.3 , #effect of knowledge of child
         zalpha = 0.1
){
  ###################
  #general properties
  ###################
  #simulate individual properties
  A <- sample ( 5 : 20, N, replace = T) #age
  
  K <- abs( rnorm (N, ak, 0.3 * ak) * if( age == T) A else runif(N, 3, 13)) #knowledge
  B <- abs( rnorm (N, ab, 0.3 * ab) * if( age == T) A else runif(N, 3, 13)) #ifelse( age == T, A, runif(N, 3, 13)) ^ rnorm (N, ab, 0.1 * ab) #abs( rnorm (N, ab, 0.3 * ab) * if( age == T) A else runif(N, 3, 13))  #body

  #simulate trip properties
  if (zero == T){
    #if zero are inflated, we're dealing with traps, hence time is in days
    L <- round ( runif(M, 1, 7) )#labor, duration of foraging trip (day)
  } else {
    #else, the trip is for shellfish collection, and the duration is in minutes
    L <- abs(rnorm(M, 150, 100)) #labor, duration of foraging trip (min)
  }
  ID_trip <- sample (1:N, size = M, replace = T)#assign trip to individual
  
  ####################
  #create outcome 
  ####################
ifelse ( zero == F,  #does it inflate zeroes?
         S <- 1,
         #either simulate from a poisson distribution
         if( distribution == "poisson"){ 
          l <-   zalpha * ( L/mean(L)) ^ zl *
                          ( K[ID_trip]/mean(K) ) ^ zk *
                          ( B[ID_trip]/mean(B) ) ^ zb
          S <- rpois( M, l ) #n success of trap
         }else{ 
         #or simulate from a binomial distribution
           if ( distribution == "binomial"){
          p <- 1 - 2 * ( 1 - inv_logit( 
                  zalpha * ( K[ID_trip]/mean(K) ) ^ zk *
                           ( B[ID_trip]/mean(B) ) ^ zb ))
          S <- rbinom( M, size = L,  p = p ) #n success of trap
         } else{
         #or simulate from a bernoulli distribution
          p <- 1 - inv_logit( 2 * ( 1 - exp(  zl * log( L/mean(L) ) +
                                              zk * log( K[ID_trip]/mean(K) ) +
                                              zb * log( B[ID_trip]/mean(B) ))))
          S <- rbern( M, p = p )
        }})
      #generate zero/not zero to have returns only when the trap yelded returns  
       notZ <- ifelse( S == 0, 0, 1)
      
      ##calculate foraging returns
      R <- notZ * rlnorm( M , (L/mean(L)) ^ (rl)  * 
                            (K[ID_trip]/mean(K)) ^ (rk) * 
                            (B[ID_trip]/mean(B)) ^ (rb) , 0.1 )  #FORAGING RETURNS
  return( list (N = N, #number of kids
                M = M, #number of trips
                A = A, #ages
                K = K, #knowledge
                B = B, #body
                L = L, #duration  of trips
                ID_trip = ID_trip, #child per trip
                #p = p, #prob success, for testing
                S = S, #success. All 1 if zero = F
                R = R  #returns amount
                ) )
}#simulation
