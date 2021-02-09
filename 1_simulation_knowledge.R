library(rethinking)



sim_know <- function (N = 100, 
                      M = 300, 
                      nact = 9, 
                      sy = 0,      #effect of years of school (between zero and one) 
                      gr = 0,      #changes the slope of the logistic regression for knowledge of individuals with activities
                      mean_g = 0,    #age dependency of a question (slope of the logistic regression - how fast it improves with x)
                      sd_g = 1, 
                      mean_b = 0,    #difficulty of a question (where the center of the slope is placed over x)
                      sd_b = 1) { 
                                                                                                                        
  
  ##tried gr = 0.1, 0.3, 1
  ##mean_g = 0, 1, 4
  ##  sd_g = 1, 0.1, 4
  ##mean_b = 0, 1, 4
  ##  sd_b = 1, 0.1, 4
  #
  
  ###people
  
  #age
  
  A <- abs(rnorm(N, 12, 3)) #age distribution more similar to real data, model doesn't seem to care
  
  
  #sex
  sex <- (rbinom (N, 1, 0.5)*2 )-1                  #give people sex coded as 1 vs -1
  
  #school years
  school_years <- rep (NA, N)                       #children get years they spent at school
  
  for (i in 1:N) {                                  #below seven, they never go to school. Over seven, they get a number of years of schooling between 0 and the total number of years if they had started at 7
    
    y <-  ifelse (A [i]< 7, 0, round(runif(1, 0, A[i]-6)))      #number of years gone to school. Will not be assigned if child below 7
    school_years[i] <- ifelse (A [i]< 7, 0, rbinom(1, 1, 0.8) *   #probabolity not to go to school at all
                                 (sex [i] + 1 +  y))           #number of sibilings reduces the school attendance (chaos with ys to avoid negative school years if child has 0 school years)
  }#N
  
    
  
  a_eff <- A + (-sy * school_years) + rnorm (N, 0, 2)
  
  
  ## Knowledge
  #easy - 1 dimension
  K <- inv_logit( gr *(a_eff- mean(A))) #uses rethinking: invert_logit, which gives you logistic function without having to write it
 
  
  
  # knowledge with many dimensions by activities
  activity_matrix <- matrix(data=NA, nrow= N, ncol=nact)                #matrix to store values (rows = people, columns = activities)
  act_skew <- rep(NA,nact)                                #to save values for further analyses
  sex_diff <- rep(NA,nact)            
  school_effect <- rep(NA,nact)      
  
  for (j in 1:nact) {
    
    act_skew[j] <- runif(1, 1, 20)                        #gives a rate of growth with age of prob performing activity
    sex_diff[j] <- rbinom(1, 1, 0.5)  * 2 - 1            #gives variability between activities in effect of sex on prob performing it (around zero so that some are more probable for boys, other for girls)
    school_effect[j] <- runif(1, 0, 1)                    #gives the proportion by which school years affect the specific activity. 0 school has no effect, 1 subtracts all years of schooling from experience
     
    for (i in 1:N) {                                     #per individual and activity, whether is performed or not. 
      p <- inv_logit( A[i]                             #probability of performing activity, in binomial below. Effect of age ()
                      - act_skew[j]                       #effect of activity
                      - school_effect[j]* school_years[i] +  #effect of schooling (effect of schooling on that activity times the number of school years of the child)
                       5* ( sex_diff[j] * sex[i] )         #effect of sex (squared to keep it positive)
                    )
      activity_matrix[i,j] <- rbinom(1, 1, p)
      
    }#N
    
  }#nact
  
  
  
  
  #create knowledge that responds to different activities
  K_1 <- inv_logit( gr * ( 1+ rowSums(activity_matrix[ ,1:5])) *(a_eff- mean(A)))
  K_2 <- inv_logit( gr * ( 1+ rowSums(activity_matrix[ ,6:7])) *(a_eff- mean(A)))
  K_3 <- inv_logit( gr * ( 1+ rowSums(activity_matrix[ ,8:9])) *(a_eff- mean(A)))
  
  
  
  
###items
  
  # M items! each has unique difficulty (b) and discrimination (g)
  
  #items belong to groups:
  item_type <- as.factor (rep( 1 : 3, length.out = M))
  
  
  g_1 <- ifelse(item_type == 1 , 1 , 0) * abs(rnorm(M, mean_g, sd_g))
  b_1 <- rnorm(M, mean_b, sd_b)
  #b_1 <- b_1 - ifelse(item_type == 1 , 1 , 0)
  
  g_2 <- ifelse(item_type == 2 , 1 , 0) * abs(rnorm(M, mean_g, sd_g))
  b_2 <- rnorm(M, mean_b, sd_b)
  #b_2 <- b_2 - ifelse(item_type == 2 , 1 , 0)
  
  g_3 <- ifelse(item_type == 3 , 1 , 0) * abs(rnorm(M, mean_g, sd_g))
  b_3 <- rnorm(M, mean_b, sd_b)
  #b_3 <- b_3 - ifelse(item_type == 3 , 1 , 0)
  
  
  
  # simulate answers
  
  Y <- matrix(NA,nrow=N,ncol=M)
  for ( i in 1:N ) for( j in 1:M ) {
    p <- inv_logit( g_1[j]*( K_1 [i] - b_1[j] ) + g_2[j]*( K_2 [i] - b_2[j] ) + g_3[j]*( K_3 [i] - b_3[j] ))
    Y[i,j] <- rbern(1,p)
  }
  
  #check correlation between knowledge and number of right answers
  
  y <- rowSums(Y)

  
  
  return(list(N = N, M = M, nact = nact, 
              A = A, a_eff = a_eff, 
              #K_1 = K_1, K_2 = K_2, K_3 = K_3,
              K = K,
              activity_matrix = activity_matrix,
              item_type = item_type,
              g_1 = g_1, g_2 = g_2, g_3 = g_3,
              b_1 = b_1, b_2 = b_2, b_3 = b_3,
              Y = Y, y = y
  ))
}
  
biglist <- sim_know()
