

#### 
#define sample sizes and replications
sample_sizes <- c(25, 50, 75, 100, 150, 200)
samples <- rep(sample_sizes, each = 30)

#create list of virtual pembas with different sample sizes
biglists <- lapply(samples, v_pemba)

#try model with intercepts only
model1 <- function(biglist = biglists[[1]]){
  #extract from list the needed information
  N <- biglist [["N"]]
  sp <- biglist [["sp"]]
  df_ID <- biglist [["df_ID"]] 
  matrix_IDactivity <- biglist [["matrix_IDactivity"]]
  df_IDsp <- biglist [["df_IDsp"]]
  
  #data list
  dat_list <- list( 
    knsp = df_IDsp$known ,
    ID = df_IDsp$ID
  ) 
  
  #model
  m1.1 <- ulam( alist(
    knsp ~ dbinom( 1 , p ) , 
    logit(p) <-  a_bar + a[ID] * sigma_a ,
    a[ID] ~ dnorm( 0 ,  1 ), 
    a_bar ~ dnorm( 0 , 1.5 ), 
    sigma_a ~ dexp(1)
  ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )
  
  #save output
  out <- precis(m1.1, depth = 2)
}


precs1 <- lapply(biglists, model1)

  
load("precs1.RData")

par(mfcol=c(2,4))
  for (i in 1:7) { plot (precs1[[i]], xlim = c( -0.5, 1))} 
  par(mfcol=c(1, 1))


  
  
  
##models with multiple input data frames#######
  
  
  
  model2 <- function(biglist = biglists[[1]]){
    
    N <- biglist [["N"]]
    sp <- biglist [["sp"]]
    df_ID <- biglist [["df_ID"]] 
    matrix_IDactivity <- biglist [["matrix_IDactivity"]]
    df_IDsp <- biglist [["df_IDsp"]]
    
  dat_list <- list( 
    knsp = df_IDsp$known ,
    ID = df_IDsp$ID ,
    age = rep(df_ID$age_st, each = sp)
  )
  
  m1.2 <- ulam( alist(
    knsp ~ dbinom( 1 , p ) , 
    logit(p) <-  a_bar + a[ID] * sigma_a + bA*age ,
    a[ID] ~ dnorm( 0 , 1  ), 
    a_bar ~ dnorm( 0 , 1.5 ), 
    bA ~ dnorm(0,1),
    sigma_a ~ dexp(1)
  ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )
  
  out <- precis(m1.2, depth = 2)
  }


  precs2 <- lapply(biglists, model2 )
  
  

####### now add in activity effects
# each activity has unique effect on knowledge of each species
# we have 0/1 dummy variables for each individual + activity
# define matrix of activity effects - AE[ activity_id , spp_id ]


  
  model8 <- function(biglist = biglists[[1]]){
    
    N <- biglist [["N"]]
    sp <- biglist [["sp"]]
    df_ID <- biglist [["df_ID"]] 
    matrix_IDactivity <- biglist [["matrix_IDactivity"]]
    df_IDsp <- biglist [["df_IDsp"]]
    
    dat_list <- list( 
      knsp = df_IDsp$known ,
      ID = df_IDsp$ID ,
      age = rep(df_ID$age_st, each = sp),
      spp_id = as.integer(df_IDsp$species),
      school_years = rep(df_ID$school_years, each = sp),
      sex = as.integer(((rep(df_ID$sex, each = sp)+1)/2)+1),
      young_sib = rep( df_ID$young_sib, each = sp),
      old_sib = rep( df_ID$old_sib, each = sp),
      adults = rep( df_ID$fam_adults, each = sp),
      HH = rep(df_ID$household, each = sp),
      am = matrix_IDactivity[rep(1:nrow(matrix_IDactivity), each = sp), ],
      N_spp = as.integer(sp)
    )
    
    m1.8 <- ulam( 
      alist(
        knsp ~ dbinom( 1 , p ) , 
        logit(p) <- a_bar + a[ID]*sigma_a + s[spp_id]*sigma_s + bA*age + bS*school_years + bY*young_sib + bO*old_sib + bAd*adults + h[HH]*sigma_h +
          #sum( AE[,spp_id[i]] * activity_matrix[i,] ) ,
          AE[ 1 , spp_id[i] ]*am[i,1] +
          AE[ 2 , spp_id[i] ]*am[i,2] +
          AE[ 3 , spp_id[i] ]*am[i,3] +
          AE[ 4 , spp_id[i] ]*am[i,4] +
          AE[ 5 , spp_id[i] ]*am[i,5] +
          AE[ 6 , spp_id[i] ]*am[i,6] +
          AE[ 7 , spp_id[i] ]*am[i,7] +
          AE[ 8 , spp_id[i] ]*am[i,8] +
          AE[ 9 , spp_id[i] ]*am[i,9] +
          AE[ 10 , spp_id[i] ]*am[i,10] ,
        a[ID] ~ dnorm( 0 , 1 ),
        a_bar ~ dnorm( 0 , 1.5 ), 
        s[spp_id] ~ dnorm( 0 , 1 ),
        h[HH] ~ dnorm( 0 , 1 ),
        bA ~ dnorm( 0 , 1 ),
        bS ~ dnorm( 0 , 1 ),
        bY ~ dnorm ( 0 , 1 ),
        bO ~ dnorm ( 0 , 1 ),
        bAd ~ dnorm ( 0 , 1 ),
        sigma_a ~ dexp( 1 ),
        sigma_s ~ dexp( 1 ),
        sigma_h ~ dexp( 1 ),
        matrix[10,N_spp]:AE ~ normal(0,1)
      ) , data=dat_list , chains=3 , cores=3 , log_lik=TRUE , sample=TRUE )
    
    out <- precis(m1.8, depth = 2)
  }
  
  
  precs8 <- mclapply(biglists, model8, mc.cores = 24)
  
  saveRDS(precs8, file = "precs8.RDS") 
  precs8 <- readRDS("precs8.RDS")
  
  precs8[sapply(precs8,function(x) all(is.character(x)))] <- NULL
  
  sds <- lapply (precs8, "[", "sd")
  precs8[[1]][["mean", 1]]
  values <- lapply(precs8,"[",c("a_bar", "bA", "bS", "bY", "bO", "bAd", "sigma_a", "sigma_s", "sigma_h"), "sd", drop=FALSE)
  
  par(mfcol=c(3,3))
  for (i in 36:44) { plot (precs8[[i]])} 
  par(mfcol=c(1, 1))
  