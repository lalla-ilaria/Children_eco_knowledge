#requires data created with v_pemba (N, sp, df_IDsp)

#to get necessary data
biglist <- v_pemba()


#add dot because writing N = N in the funciton is a problem, apparently
N. <- biglist [["N"]]
sp. <- biglist [["sp"]]
df_IDsp. <- biglist [["df_IDsp"]]


#add time of listing

#function to give time stamp to species
timed_pemba <- function ( N = N. , sp = sp. , df_IDsp = df_IDsp. ) {

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
    for (i in 2:nrow(df_IDsp)) {                                                 #adds longer lag between last item in a group and first in different group
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
      
    
        return( list(
              freelists = df_IDsp,
              time_dfs = time_dfs ))

}#time_pemba


freebiglist <- timed_pemba()

freelists <- freebiglist[['freelists']]
time_dfs <-  freebiglist[['time_dfs']]
