#Simulate data to conduct mock analyses to implement the statistical model
#Causal relations between traits reflect those described in the associated DAG

#load required packages
library(rethinking)

#define function to simulate knowledge
sim_know <- function (N = 30,        #number of individuals
                      M = 100,       #number of questions
                      H=floor(N/5),  #number of households
                      nact = 1,      #number of activites individuals can perform
                      b_A = 0,       #direct effect of age on knowledge
                      b_OS = 0,      #effect of older siblings
                      b_YS = 0,      #effect of younger siblings
                      b_ad = 0,      #effect of adults
                      b_sy = 0,      #effect of years of school (between zero and one) 
                      b_ac = 0,      #effect of activities
                      eta_a = 1.5,   #age dependency of a question (slope of the logistic regression - how fast it improves with x) -exp distributed
                      mean_b = 0,    #difficulty of a question (where the center of the slope is placed over x)
                      sd_b = 1,
                      alpha_c = 0,   #guessing probability (make alpha zero to remove this element, make 5 to have an effect of guessing) - beta distributed
                      beta_c = 5,
                      n_dimensions = 1) { #either one or more, creates answers that respond differently to activities
                                                                                                                        
#########
###People
#########  
  ##age
  #age distribution similar to real data, not uniform
  A <- abs(rnorm(N, 12, 3)) 
  
  ##sex
  #gives people sex coded as 1 vs -1
  sex <- (rbinom (N, 1, 0.5)*2 )-1 
  
  ##family
  #children belong to a household
  household <- round(runif(N, 1, H)) 
  #each household has a number of adults (> 20) 
  hh_adults <- 1 + rbinom (H, 1, 0.8) + rbinom (H, 1, 0.4) + rbinom (H, 1, 0.2) 
  fam_adults <- rep (NA, N)
  for (i in 1:N) {
    fam_adults[i] <- hh_adults[household[i]]  
  }
  #number of older children in the household per child
  old_sib <- rep (NA, N)                            
  for (i in 1:N) {
    old_sib[i] <- sum(household == household[i] & 
                        A > A[i] ,na.rm = TRUE)
  }
  #number of younger children in the household per child
  young_sib <- rep (NA, N)                          
  for (i in 1:N) {
    young_sib[i] <- sum(household == household[i] & 
                          A < A[i] ,na.rm = TRUE)
  }
  
  
  ##school years
  #children get years they spent at school
  #below seven, they never go to school. 
  #Over seven, they get a number of years of schooling between 0 and the total number of years if they had started at 7
  #some kids do not attend school at all. Some others do not invest much time and never move over third grade or so
  school_years <- rep (NA, N)      
  
  for (i in 1:N) {                 
    y <-  ifelse (A [i]< 7, 0, round(runif(1, 0, A[i]-6)))        #number of years gone to school. Will not be assigned if child below 7
    school_years[i] <- ifelse (A [i]< 7, 0, rbinom(1, 1, 0.8) *   #probabolity not to go to school at all
                                 (sex [i] + 1 +  y))              #number of sibilings reduces the school attendance (chaos with ys to avoid negative school years if child has 0 school years)
  }#N
  
  
  ## activities
  #Children perform any activity with higher probability as they get older
  #there is a sex difference in the probability they perform each activity
  activity_matrix <- matrix(data=NA, nrow= N, ncol=nact)  #matrix to store values (rows = people, columns = activities)
  act_skew <- rep(NA,nact)                                #to save values for activity simulatin
  sex_diff <- rep(NA,nact)            
  school_effect <- rep(NA,nact)      
  
  #define charactieristics for each activity
  for (j in 1:nact) {
    act_skew[j] <- runif(1, 1, 20)                        #gives a rate of growth with age of prob performing activity
    sex_diff[j] <- rbinom(1, 1, 0.5)  * 2 - 1             #gives variability between activities in effect of sex on prob performing it (around zero so that some are more probable for boys, other for girls)
    school_effect[j] <- runif(1, 0, 1)                    #gives the proportion by which school years affect the specific activity. 0 school has no effect, 1 subtracts all years of schooling from experience
  
  #assign 1 to individuals if the activity is performed  
    for (i in 1:N) {                                      #per individual and activity, whether is performed or not. 
       p <- inv_logit( A[i]                               #probability of performing activity, in binomial below. Effect of age ()
                      - act_skew[j]                       #effect of activity
                      - school_effect[j]* school_years[i] + #effect of schooling (effect of schooling on that activity times the number of school years of the child)
                       5* ( sex_diff[j] * sex[i] )        #effect of sex (squared to keep it positive)
                    )
      activity_matrix[i,j] <- rbinom(1, 1, p)
      }#N
  }#nact
  
############  
###Knowledge
############
  #Create n_dimensions of knowledge, with effect of activities on dimensions
  K <- matrix(NA, nrow = N, ncol = n_dimensions)
  #divide activities per each dimension
  acteff  <- rep(1:n_dimensions, length.out = nact) 
  
  #assign knowledge to individuals incliuding all factors
  for (i in 1:n_dimensions) {
    K[,i] <-  b_A * standardize (A) +
              b_sy * standardize(school_years) + #adds the effect of school years
              b_OS * standardize(old_sib) + b_YS * standardize(young_sib) + b_ad * standardize(fam_adults) + #effect of families
              apply(t ( t (activity_matrix[ ,which(acteff == i),drop = FALSE]) * b_ac), 1, sum) + #sums up the effect of each activity - as multiplied by activity coefficient
              rnorm (N, 0, 0.5) #
  }
  # #for making knowledge depend non linearly from age and explore this with model m_ord_age
  # for (i in 1:n_dimensions) {
  #   K[,i] <- standardize(A ^ b_A) + rnorm (N, 0, 0.5)
  # }

  

########  
###Items
########
  # M items, each has unique difficulty (b) and discrimination (a)
  item_type <- as.factor (rep( 1 : n_dimensions, length.out = M)) #assigns item to different groups which are associated to dimensions of knowledge
  a <- matrix(NA, nrow = M, ncol = n_dimensions) #discrimination
  b <- matrix(NA, nrow = M, ncol = n_dimensions) #difficulty
  c <- matrix(NA, nrow = M, ncol = n_dimensions) #pseudoguessing
  
  for(i in 1:n_dimensions){
    a[,i] <- ifelse(item_type == i , 1 , 0.1) * rexp(M, eta_a)
    b[,i] <- ifelse(item_type == i ,rnorm(M, mean_b, sd_b) , 3)  
    c[,i] <- rbeta(M, alpha_c, beta_c)
  }
  
##########
###Answers
##########
  
  #select p with either one or multiple dimensions
  Y <- matrix(NA,nrow=N,ncol=M)
  for ( i in 1:N ) for( j in 1:M ) {
    p <- 0
    for(k in 1:n_dimensions){
    p <-  p + a[j, k]*( K[i, k] - b[j, k] )
  }
    Y[i,j] <- rbern(1,c[j]+(1-c[j])* inv_logit(p))
  }
  
 
  
  #check correlation between knowledge and number of right answers
  
  y <- rowSums(Y)

#########
###Output
#########
  return(list(N = N, M = M, nact = nact, 
              A = A, S = sex, SY = school_years, 
              HH = household, Nad = fam_adults, OS = old_sib, YS = young_sib,
              activity_matrix = activity_matrix,
              K = K,
              item_type = item_type,
              a = a, b = b, c = c,
              Y = Y,
              n_dimensions = n_dimensions
  ))
}

