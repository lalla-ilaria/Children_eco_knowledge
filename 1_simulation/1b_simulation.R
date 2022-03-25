sim_data <- function (N , M ,    #number of 
          zero = T,
          b_ak = 0.4,
          g_ak = 0.4,
          b_a = 2,
          g_a = 2,
          b_k = 2,
          g_k = 2,
          l = 1){
  #parameters
  alpha <- rexp(1, 1)
  beta_a <- abs(rnorm(N, b_a, 0.2))
  gamma_a <- abs(rnorm(N, g_a, 0.2))
  beta_k <- abs(rnorm(N, b_k, 0.2))
  gamma_k <- abs(rnorm(N, g_k, 0.2))
  beta_ak <- abs(rnorm(N, b_ak, 0.1))
  gamma_ak <- abs(rnorm(N, g_ak, 0.1))
  lambda <-  rexp(1,l)
  sigma <- rexp(1, 1)
  
  #Simulate individual traits
  AGE <- runif(N,3,20)
  K <- vector("numeric", length = N)
  for(i in 1:N){
    K[i] <- (1-exp(-beta_ak[i]*AGE[i]/mean(AGE)))^gamma_ak[i] #trait
  }
  #save individual total effects
  phi <- vector("numeric", length = N) 
  for(i in 1:N){
     phi[i] <- (1-exp(-beta_a[i] * AGE[i]/mean(AGE)  )) ^ gamma_a[i] +
                     (1-exp(-beta_k[i] * K[i])) ^ gamma_k[i]
  }
  #simulate trip properties
  if (zero == T){
    #if zero are inflated, we're dealing with traps, hence time is in days
    L <- round ( runif(M, 1, 7) )#labor, duration of foraging trip (day)
  } else {
    #else, the trip is for shellfish collection, and the duration is in minutes
    L <- abs(rnorm(M, 150, 100)) #labor, duration of foraging trip (min)
  }
  #create matrixes to save trip effects
  psi <- vector("numeric", length = M) 
  psi <-   lambda * log (L/mean(L))
  
  ID_ind <- sample (1:N, size = M, replace = T)#assign trip to individual
  
  #calculate per datapoint
  p <- vector("numeric", length = M) 
  S <- vector("numeric", length = M) 
  R <- vector("numeric", length = M) 
  for(i in 1:M){
        if( zero == F) S <- rep(1, M) else {
            p[i] <- 1 - exp ( abs(alpha) * phi[ID_ind[i]] * - exp(psi[i]) )
            S[i] <- rbern(1, p[i])
            }     
        m <- alpha + phi[ID_ind[i]] + psi[i]
        R[i] <- S[i] * rlnorm (1, m, sigma)
  }

  return( list (N = N, #number of kids
                M = M, #number of trips
                A = AGE, #ages
                K = K, #knowledge
                L = L, #duration  of trips
                ID_ind = ID_ind, #child per trip
                p = p, #prob success, for testing
                S = S,
                R = R  #returns amount
                ) )
}





