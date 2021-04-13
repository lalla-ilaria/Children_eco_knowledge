###FUNCTIONS TO WRANGLE DATA

#CHECK consistency of IDs
ID_check <- function( df  , column , reference) {
  #find all IDs not present in census
  IDs <- vector(length = nrow(df))
  for (i in 1:nrow(df)) { 
    IDs[i] <-  df[i, column] %in% reference
  }
  IDs_to_check <-df[which(IDs == FALSE), c(1:3)]
  if(any(IDs == FALSE)) warning (paste("Check '", column, "' of", df[which(IDs == FALSE), 1] ))
  return( IDs_to_check)
}

#ANONYMYZE IDs
  #original IDs are composed of 5 letters from the name and 4 random numbers.
  #the anonymized IDs retain the 4 random numbers, preceded by one or two digits corresponiding to the first letter of the original ID (to ensure consistency across datasets)
anonyme <- function( df , column) {
  anonyme_id <- vector( length = nrow(df) )
  for(i in 1:nrow(df)){
    anonyme_id[i] <- paste( which(letters == tolower(substring(df[i,column], 1, 1))), parse_number(df[i,column]), sep = "")
  }
  return(anonyme_id)
}
