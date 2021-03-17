data <- read.csv("census 18.10.17.csv")



# create family variables for each child who lives in BK

#with number of adults per household (individuals >=20 who live in BK per household)
for (i in 1:nrow(data)) {
  data$hhadults[i] <- ifelse (data$age[i] <= 20 & data$lives.bandari.kuu [i] == "yes",
                        sum(data$household == data$household[i] & 
                            (data$age >= 20 | data$role_in_household == "mother" ) &
                            data$lives.bandari.kuu == "yes", 
                            na.rm = TRUE), NA)
}

#number of older sibilings (children of the mother older than focal)
for (i in 1:nrow(data)) {
  data$oldsib[i] <- ifelse (data$lives.bandari.kuu [i] == "yes",
                      sum(data$mother == data$mother[i] & 
                          data$age > data$age[i] , 
                          na.rm = TRUE), NA)
}

#number younger sibilings (children of the mother younger than focal)
for (i in 1:nrow(data)) {
  data$youngsib[i] <- ifelse (data$lives.bandari.kuu [i] == "yes",
                        sum(data$mother == data$mother[i] & 
                            data$age < data$age[i] , 
                            na.rm = TRUE), NA)
}

#NB include people outside of bandari kuu?Maybe just count order in the sibship


