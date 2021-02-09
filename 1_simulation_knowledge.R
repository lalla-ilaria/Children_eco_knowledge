library(rethinking)



sim_know <- function (N = 100, 
                      M = 300, 
                      nact = 9, 
                      sy = 0,      #effect of years of school (between zero and one) 
                      gr = 0.3,      #changes the slope of the logistic regression for knowledge of individuals 
                      mean_g = 0,    #age dependency of a question (slope of the logistic regression - how fast it improves with x)
                      sd_g = 1, 
                      mean_b = 0,    #difficulty of a question (where the center of the slope is placed over x)
                      sd_b = 1,
                      n_dimensions = 1) { #either one or more, creates answers that respond differently to activities
                                                                                                                        
#########
###People
#########  
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
  
  #effective age calculated with school years
  a_eff <- A + (-sy * school_years) + rnorm (N, 0, 2)
  
  # activities
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
  
############  
###Knowledge
############
  #Create n_dimensions of knowledge, with effect of activites on dimensions
  K <- matrix(NA, nrow = N, ncol = n_dimensions)
  acteff  <- rep(1:3, length.out = nact)
  for (i in 1:n_dimensions) {
    K[,i] <- inv_logit( gr * ( 1+ rowSums(activity_matrix[ ,which(acteff == i)])) *(a_eff- mean(A)))
  }
  
########  
###Items
########
  # M items! each has unique difficulty (b) and discrimination (g)

  item_type <- as.factor (rep( 1 : n_dimensions, length.out = M))
  g <- matrix(NA, nrow = M, ncol = n_dimensions)
  b <- matrix(NA, nrow = M, ncol = n_dimensions)
  for(i in 1:n_dimensions){
  g[,i] <- ifelse(item_type == i , 1 , 0) * abs(rnorm(M, mean_g, sd_g))
  b[,i] <- rnorm(M, mean_b, sd_b)
  }
  
##########
###Answers
##########
  
  #select p with either one or multiple dimensions
  Y <- matrix(NA,nrow=N,ncol=M)
  for ( i in 1:N ) for( j in 1:M ) {
    sum(for(k in 1:n_dimensions){
    p <- inv_logit( g[j, k]*( K[i, k] - b[j, k] ))
  }
    )
    Y[i,j] <- rbern(1,p)
  }
  
 
  
  #check correlation between knowledge and number of right answers
  
  y <- rowSums(Y)

#########
###Output
#########
  return(list(N = N, M = M, nact = nact, 
              A = A, a_eff = a_eff, 
              K = K,
              activity_matrix = activity_matrix,
              item_type = item_type,
              g = g, b = b,
              Y = Y, y = y,
              n_dimensions = n_dimensions
  ))
}
  

