

#################################
########analysis-models##########
#################################

## ---- model


##m1.1 basic model-intercept only#################

#data list
dat_list <- list( 
  knsp = df_IDsp$known ,
  ID = df_IDsp$ID
  ) 

m1.1 <- ulam( alist(
  knsp ~ dbinom( 1 , p ) , 
  logit(p) <-  a_bar + a[ID] * sigma_a ,
  a[ID] ~ dnorm( 0 ,  1 ), 
  a_bar ~ dnorm( 0 , 1.5 ), 
  sigma_a ~ dexp(1)
) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )

precis(m1.1, depth = 2)








#m1.2 add age#######

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

precis(m1.2, depth = 2)






#individuals, species and age######

#data
dat_list <- list( 
  knsp = df_IDsp$known ,
  ID = df_IDsp$ID ,
  age = rep(df_ID$age_st, each = sp),
  spp_id = as.integer(df_IDsp$species)
)

m1.3 <- ulam( alist(
  knsp ~ dbinom( 1 , p ) , 
  logit(p) <-  a_bar + a[ID] * sigma_a + s[spp_id] * sigma_s + bA*age ,
  a[ID] ~ dnorm( 0 , 1 ), 
  a_bar ~ dnorm( 0 , 1.5 ), 
  s[spp_id] ~ dnorm( 0 , 1 ),
  bA ~ dnorm(0,1),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1)
) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )


precis(m1.3, depth = 2)



#individuals, age, species, school#######

#data
dat_list <- list( 
  knsp = df_IDsp$known ,
  ID = df_IDsp$ID ,
  age = rep(df_ID$age_st, each = sp),
  spp_id = as.integer(df_IDsp$species),
  school_years = rep(df_ID$school_years, each = sp)
)

m1.5 <- ulam( alist(
  knsp ~ dbinom( 1 , p ) , 
  logit(p) <-  a_bar + a[ID] * sigma_a + s[spp_id] * sigma_s + bA*age + bS*school_years ,
  a[ID] ~ dnorm( 0 , 1 ),  # hyper-priors
  a_bar ~ dnorm( 0 , 1.5 ), 
  s[spp_id] ~ dnorm( 0 , 1 ),
  bA ~ dnorm(0,1),
  bS ~ dnorm(0,1),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1)
) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )


precis(m1.5, depth = 2)


#add effect of sex#######

#data
dat_list <- list( 
  knsp = df_IDsp$known ,
  ID = df_IDsp$ID ,
  age = rep(df_ID$age_st, each = sp),
  spp_id = as.integer(df_IDsp$species),
  school_years = rep(df_ID$school_years, each = sp),
  sex = as.integer(((rep(df_ID$sex, each = sp)+1)/2)+1)
)

m1.6 <- ulam( alist(
  knsp ~ dbinom( 1 , p ) , 
  logit(p) <-  a_bar + a[ID] * sigma_a + s[spp_id] * sigma_s + bA*age + bS*school_years + ex[sex] * sigma_ex ,
  a[ID] ~ dnorm( 0 , 1  ),
  a_bar ~ dnorm( 0 , 1.5 ), 
  s[spp_id] ~ dnorm( 0 , 1 ),
  bA ~ dnorm( 0 , 1 ),
  bS ~ dnorm( 0 , 1 ),
  ex[sex] ~ dnorm( 0 , 1 ),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1),
  sigma_ex ~ dexp(1)
) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )


precis(m1.6, depth = 2)



#data for model
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
  HH = rep(df_ID$household, each = sp)
)


#family#####
m1.7 <- ulam( alist (
  knsp ~ dbinom( 1 , p ) , 
  logit(p) <-  a_bar + a[ID] * sigma_a + s[spp_id] * sigma_s + bA*age + ex[sex] * sigma_ex + bS*school_years + bY*young_sib + bO*old_sib + bAd*adults +  h[HH] * sigma_h,
  a[ID] ~ dnorm( 0 , 1 ),
  a_bar ~ dnorm( 0 , 1.5 ), 
  s[spp_id] ~ dnorm( 0 , 1 ),
  h[HH] ~ dnorm( 0 , 1 ),
  bA ~ dnorm( 0 , 1 ),
  bS ~ dnorm( 0 , 1 ),
  bY ~ dnorm ( 0 , 1 ),
  bO ~ dnorm ( 0 , 1 ),
  bAd ~ dnorm ( 0 , 1 ),
  ex[sex] ~ dnorm( 0 , 1 ),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1),
  sigma_ex ~ dexp(1),
  sigma_h ~ dexp(1)
) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )


precis(m1.7)



####### now add in activity effects
# each activity has unique effect on knowledge of each species
# we have 0/1 dummy variables for each individual + activity
# define matrix of activity effects - AE[ activity_id , spp_id ]

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
  N_spp = as.integer( max(df_IDsp$species) )
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
  ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE , sample=TRUE )
compare(m1.1, m1.2, m1.3, m1.5, m1.6, m1.7, m1.8)



