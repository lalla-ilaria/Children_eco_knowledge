# define simulation function
# takes various study design arguments and simulates a data set for Virtual Pemba

library(rethinking)


#creates a data frame with individual level information per children
v_pemba <- function( N = 50 , H=floor(N/5), school_effsib=4,
                    nact=10,	b_sexdiff=3 ,	b_sibs=0 ,
                    sp=100,
                    school_eff = 0.3, adult_eff = 0.2,  oldsib_eff = 0.6, youngsib_eff = 0.5) {
  
  	
  	#Age
  	age <- runif (N, 5, 20)                           #give people age between 5 and 20 years
  
  	#Sex
  	sex <- (rbinom (N, 1, 0.5)*2 )-1                  #give people sex coded as 1 vs -1
  
  
  	## ---- household_composition
  
  	#Fam
  	household <- round(runif(N, 1, H))                #children belong to a household
  
  	hh_adults <- 1 + rbinom (H, 1, 0.8) + rbinom (H, 1, 0.4) + rbinom (H, 1, 0.2) #each household has a number of adults (> 20) 
  
  	fam_adults <- rep (NA, N)
  	  for (i in 1:N) {
  	    fam_adults[i] <- hh_adults[household[i]]  
  	  }
  
  	old_sib <- rep (NA, N)                            #number of older children in the household per child
  	  for (i in 1:N) {
  	    old_sib[i] <- sum(household == household[i] & 
  	                    age > age[i] ,na.rm = TRUE)
  	  }
  
  	young_sib <- rep (NA, N)                          #number of younger children in the household per child
  	  for (i in 1:N) {
  	    young_sib[i] <- sum(household == household[i] & 
  	                       age < age[i] ,na.rm = TRUE)
  	  }
  
     # now school
  
  	school_years <- rep (NA, N)                       #children get years they spent at school
  	      
  	for (i in 1:N) {                                  #below seven, they never go to school. Over seven, they get a number of years of schooling between 0 and the total number of years if they had started at 7
  	      
  	      y <-  ifelse (age [i]< 7, 0, round(runif(1, 0, age[i]-6)))      #number of years gone to school. Will not be assigned if child below 7
  	      s <- (young_sib[i] + old_sib[i])/school_effsib
  	      school_years[i] <- ifelse (age [i]< 7, 0, rbinom(1, 1, 0.8) *   #probabolity not to go to school at all
  	                                  (sex [i] + 1 -                      #one sex gets on average +2 years of schooling
  	                                  ifelse(s < y, s, 0)+  y))           #number of sibilings reduces the school attendance (chaos with ys to avoid negative school years if child has 0 school years)
  	}#N
  
  	
  	
  	
  	
  	
  	#create vertical df 
  	df_ID <- data.frame(ID = 1:N,
  	                 age = as.integer (age),
  	                 sex = as.integer (sex),
  	                 household = as.integer (household),
  	                 fam_adults = as.integer (fam_adults),
  	                 old_sib = as.integer (old_sib),
  	                 young_sib = as.integer (young_sib),
  	                 school_years = as.integer (school_years))
  	df_ID$age_st <- standardize(df_ID$age)                    #add column for standardized age
  	


	
	
## ---- activities
#dependent variable: activity (act). As a function of age, sex, schooling (and family)
#Age -> Act 
#Sex -> Act 
#Sch -> Act



	
  	activity_matrix <- matrix(data=NA, nrow= N, ncol=nact)                #matrix to store values (rows = people, columns = activities)
  	  act_skew <- rep(NA,nact)                                #to save values for further analyses
  	  sex_diff <- rep(NA,nact)            
  	  school_effect <- rep(NA,nact)      
  	  sib_effect <- rep(NA,nact)  
  
  	for (j in 1:nact) {
  	  
  	    act_skew[j] <- runif(1, 1, 20)                        #gives a rate of growth with age of prob performing activity
  	    sex_diff[j] <- rbeta(1, 0.5, 0.5)  * 2 - 1            #gives variability between activities in effect of sex on prob performing it (around zero so that some are more probable for boys, other for girls)
  	    school_effect[j] <- runif(1, 0, 1)                    #gives the proportion by which school years affect the specific activity. 0 school has no effect, 1 subtracts all years of schooling from experience
  	    sib_effect[j] <- runif(1,-0.1, 0.1)                   #mediates the effect of sibship size on activities performed. some are more probable with more siblings, other less common
  	    
  	    for (i in 1:N) {                                     #per individual and activity, whether is performed or not. 
  	      p <- inv_logit( df_ID$age[i]                             #probability of performing activity, in binomial below. Effect of age ()
  	                      - act_skew[j]                       #effect of activity
  	                      - school_effect[j]*df_ID$school_years[i] +  #effect of schooling (effect of schooling on that activity times the number of school years of the child)
  	                      b_sexdiff*( sex_diff[j] * df_ID$sex[i] ) +        #effect of sex (squared to keep it positive)
  	                      b_sibs*sib_effect[j]*(df_ID$young_sib[i]+df_ID$old_sib[i])#effect of sibilings does not work. To be implemented
  	              )
  	      activity_matrix[i,j] <- rbinom(1, 1, p)
  	      
  	    }#N
  
  	}#nact
 



#dependent variable
	## ---- plot_probspecies

#effect of activities
	acteff <- runif (nact, -2, 4)                    #gives an effect to each activity, in knowledge-years gained 

	spact <- runif (sp, 0, 1)                        #moduates the effect of activities in a different way per species
	
	eff_act <- matrix(NA, nrow = sp, ncol = nact)    #creates matrix where to store effect of each activity on each species knowledge
	
	for (i in 1:sp) {
	  for (j in 1:nact) {
	    eff_act[i,j] <- acteff[j] * spact[i]
	  }#nact
	}#sp
	
  




	#give each species sp a probability of being known at different ages
	sp_curve <- matrix(data=NA, nrow= sp, ncol=45)
	  year_spp <- rep (NA, sp)

	for (j in 1:sp) {
	  year_spp[j] <- runif (1, 1, 25)
	          for (i in 1:45) {
	            sp_curve[j,i] = plogis(i, year_spp[j] , 7)                       #each species i gets an average of normal distribution between 1 and 25 and, for each year between 1 and 25, a probability of being known
	          }
	}#sp



	p_IDsp <- matrix(data=NA, nrow= N, ncol=sp)

	known_IDsp <- matrix(data=NA, nrow= N, ncol=sp)
	for (i in 1:N) {
	  add_age <- school_eff * df_ID$school_years [i] +                    #a proportion of the years spent in school is subtracted from the actual age. School reduces knowledge
	            (df_ID$fam_adults[i] - (adult_eff*df_ID$fam_adults[i]))+  #each adult in the household increases effective age with decreasing effect of additional adults
	            (df_ID$old_sib[i] - (oldsib_eff*df_ID$old_sib[i]))-       #each older sibiling increases the effective age
	            (df_ID$young_sib[i] - (youngsib_eff*df_ID$young_sib[i]))  #each older sibiling decreases the effective age
	  act_effect <-  eff_act%*%diag(activity_matrix[i,])
	  act_effect <- rowSums(act_effect)
	     for (j in 1:sp) {
	           p_IDsp[i,j] <- sp_curve[ j, df_ID$age[i]+add_age+act_effect[j] ]
	           known_IDsp [i,j] = rbinom (1, 1 , p_IDsp[i,j])                  
	         }  
	  
	}#N
	
	
	
	df_IDsp <- data.frame(ID =  rep (1:N, each = sp),
	                      species = rep(1:sp, N),                              #give number of each species to row
	                      known = as.vector(t(known_IDsp)))

	                      
	  #add column with proportion of species known by individuals                   	                      
    for (i in 1:N) {
    	    df_ID$prop_known [i] <- (sum(  df_IDsp$ID == i & df_IDsp$known == 1))/sp
    }
	
	
	
	#add time of listing
	
	#prepare data
	df_IDsp$order <- ifelse(df_IDsp$known == 1, runif( sum(df_IDsp$known == 1) , 0.5 , 1.5), NA) #gives random order of listing items
	
	df_IDsp$group_sp <-abs( df_IDsp$species - round(df_IDsp$species, -1))
	
	df_IDsp$order <- ifelse(df_IDsp$known == 1, df_IDsp$group_sp + df_IDsp$order, NA)
	
	df_IDsp <- df_IDsp[order(df_IDsp$ID, df_IDsp$order ),]  #orders the df in order by appropriate column, to assign times accordingly
	
	
	#numbers the ordered species accordingly
	ord <- rep(1:sp, N)                                     
	
	for (i in 1:nrow(df_IDsp)) {
	  
	  df_IDsp$order[i] <- ifelse( is.na(df_IDsp$order[i])==TRUE, NA, ord[i] )
	}
	rm(ord)
	
	#gives random distance between items listed in order
	for (i in 1:nrow(df_IDsp)) {                           
	  
	  df_IDsp$time_lag[i] <- ifelse(is.na(df_IDsp$order[i])==TRUE, NA, rgamma(1, 3, 2))
	  
	  df_IDsp$xmin[i] <- ifelse(is.na(df_IDsp$order[i])==TRUE, NA, df_IDsp$time_lag)          #to give initial lag to first named object
	  
	}#nrow(df_IDsp)
	
	
	df_IDsp$word_length <- ifelse(is.na(df_IDsp$order)==TRUE, NA, rgamma(sum(df_IDsp$known == 1), 6, 6)) #creates length of words
	df_IDsp$xmax <- ifelse(is.na(df_IDsp$order)==TRUE, NA, df_IDsp$xmin + df_IDsp$word_length)
	
	  #calculates the starting and finishing time of each item
	  for (i in 2:sum(df_IDsp$known == 1 )) {                                                 #adds longer lag between last item in a group and first in different group
	    df_IDsp$time_lag[i] <- ifelse(is.na(df_IDsp$order[i])==TRUE, NA, 
	                                  ifelse(df_IDsp$group_sp[i] ==  df_IDsp$group_sp[i-1], df_IDsp$time_lag[i], 
	                                         df_IDsp$time_lag[i] + rgamma(1, 15, 3)))
	                                  
	    df_IDsp$xmin[i] <- ifelse(df_IDsp$ID[i] == df_IDsp$ID[i-1],                            #gives time of start of words
	                              df_IDsp$xmin[i-1] + df_IDsp$word_length[i-1] + df_IDsp$time_lag[i] ,
	                              df_IDsp$time_lag[i])
	    
	  df_IDsp$xmax[i] <- df_IDsp$xmin[i] + df_IDsp$word_length[i]
	  
	  }#sum(df_IDsp$known == 1 ))
	  
	  

	
	
	  	  
	#calculate distance between terms
    
	  #create lsit of empty data frames
	time_dfs <- 	  rep( list(
	  data.frame( 
	    ID = rep(NA, sp^2),
	    sp1 = rep(NA, sp^2),
	    sp2 = rep(NA, sp^2),
	    xmin_sp1 = rep(NA, sp^2),
	    xmin_sp2 = rep(NA, sp^2),
	    species = rep(NA, sp^2),
	    time_dist = rep(NA, sp^2)
	  )
	  ), N)

	  #fills df per individual with distance between items    
	for (n in 1:N ) {
	  
	  IDdf <- subset(df_IDsp, df_IDsp$ID == n)
	  
	  time_dfs[[n]][,1] <-  rep(unique(IDdf$ID), sp*sp)
	  time_dfs[[n]][,2] <-  rep(IDdf$species, sp)
	  time_dfs[[n]][,3] <-  rep(IDdf$species, each = sp)
	  time_dfs[[n]][,4] <-  rep(IDdf$xmin, sp)
	  time_dfs[[n]][,5] <-  rep(IDdf$xmin, each = sp)
	  time_dfs[[n]][,6] <-  as.integer(paste(rep(IDdf$species, sp), rep(IDdf$species, each = sp), sep=""))
	  time_dfs[[n]][,7] <- abs(time_dfs[[n]][,4]-time_dfs[[n]][,5])  
	  
	  time_dfs[[n]] <- time_dfs[[n]][complete.cases(time_dfs[[n]]), ]
	}
	
	#binds data frames to have all info per all individuals in one
	df_time <- do.call("rbind", time_dfs)
		
	# RESULT
	return(list(N = N,
	            sp = sp,
	            df_ID = df_ID, 
	            matrix_IDactivity = activity_matrix , 
	            df_IDsp = df_IDsp,
	            p_IDsp = p_IDsp,
	            effect_sp_act = eff_act,
	            sp_curve = sp_curve,
	            df_time = df_time))
	

	
	
	
	
	}#v_pemba


biglist <- v_pemba()


#extract from list
N <- biglist [["N"]]
sp <- biglist [["sp"]]
df_ID <- biglist [["df_ID"]] 
matrix_IDactivity <- biglist [["matrix_IDactivity"]]
df_IDsp <- biglist [["df_IDsp"]]
p_IDsp <- biglist [["p_IDsp"]]
effect_sp_act <- biglist [["effect_sp_act"]]
sp_curve <- biglist[["sp_curve"]]
df_time <- biglist [["df_time"]]
 
