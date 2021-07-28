#the following script is not executable, included only for transparency

#Load packages
library(tidyverse)

#loads functions
if( !exists( "ID_check", mode = "function")) source( "2_Data_preparation/functions.r")

######################
#LOAD FILES###########
######################

#Load data
#xxx to the folder where original data are located
#census as a reference
census <- read.csv("xxx/census_BK_2020.csv")
#interviews
interviews <- read.csv("xxx/interviews.csv")
#load freelists
freelists <- read.csv("xxx/freelists.csv")
#questions - long template
ql <- read.csv("xxx/quiz_responses.csv")
#questions - short template 
qs <- read.csv("xxx/quiz_responses.csv")
#picture recognition
recognition <- read.csv("xxx/recognized_items.csv")
#chores
chores <- read.csv("xxx/chores.csv")
#household survey #1 and#2
demography <- read.csv("xxx/household_members_demography.csv")
survey_act <- read.csv("xxx/household_members_activities.csv")
#reproductive history
children <- read.csv("xxx/3_relational_tables/children.csv")
#Households distance from researcher's station
h_dist <- read.csv("xxx/houses_dist.csv")
#correction for names
if( !exists( "N", mode = "data.frame")) source( "xxx/correction_names_HHinteviews.r")


######################
#CORRECT IDs##########
######################

#function ID_check reports entries which ID has been transcribed incorrectly 
#incorrect entries are individually corrected via script
#interviews
ID_check(df = interviews, column = "person_id", census$ID)#incorrect entries are listed
interviews$person_id[which(interviews$person_id == "incorrect_id")] <- "correct_id" #incorrect entries are individually corrected
interviews$person_id[which(interviews$person_id == "incorrect_id")] <- "correct_id"
ID_check(df = interviews, column = "person_id", census$ID)#no incorrect entries after individual correction

#freelists
ID_check(df = freelists, column = "person_id", interviews$person_id)
freelists$person_id[which(freelists$person_id == "incorrect_id")] <- "correct_id"
freelists$person_id[which(freelists$person_id == "incorrect_id")] <- "correct_id"
freelists$person_id[which(freelists$person_id == "incorrect_id")] <- "correct_id"
ID_check(df = freelists, column = "person_id", interviews$person_id)

#questions - long template
ID_check(df = ql, column = "person_id", interviews$person_id)
ql$person_id[which(ql$person_id == "incorrect_id")] <- "correct_id"
ID_check(df = ql, column = "person_id", interviews$person_id)

#questions - short template
ID_check(df = qs, column = "person_id", interviews$person_id)
qs$person_id[which(qs$person_id == "incorrect_id")] <- "correct_id"
ID_check(df = qs, column = "person_id", interviews$person_id)

#picture recognition
ID_check(df = recognition, column = "person_id", interviews$person_id)
recognition$person_id[which(recognition$person_id == "incorrect_id")] <- "correct_id"
ID_check(df = recognition, column = "person_id", interviews$person_id)

#chores
ID_check(df = chores, column = "person_id", interviews$person_id)
chores$person_id[which(chores$person_id == "incorrect_id")] <- "correct_id"
chores$person_id[which(chores$person_id == "incorrect_id")] <- "correct_id"
ID_check(df = chores, column = "person_id", interviews$person_id)

#check and correct household IDs
ID_check(df = interviews, column = "hh_id", census$household_2019)
interviews$hh_id[which(interviews$person_id == "incorrect_id")] <- "correct_id"
interviews$hh_id[which(interviews$person_id == "incorrect_id")] <- "correct_id"
ID_check(df = interviews, column = "hh_id", census$household_2019)

interviews$hh_id <- parse_number(interviews$hh_id)

##############
#SEX##########
##############

#add sex from census
for(i in 1:nrow(interviews)){
interviews$sex[i] <- census[which (census$ID == interviews$person_id[i]), "sex"]
}

######################
#ACTIVITIES IN HHS####
######################

#merge pages 1 and 2 of household survey
demography[which (demography$demography_row_number >= 13),"demography_row_number"] <- demography[which (demography$demography_row_number >= 13),"demography_row_number"] -12
demography$merge <- paste(demography$pdf_hash, demography$demography_row_number)
survey_act$merge <- paste(survey_act$pdf_hash, survey_act$activities_row_number)
hhs <- merge(demography, survey_act, by = "merge", all = TRUE, sort = FALSE)

hhs <- hhs[ -which(is.na(hhs$hhm_name) ),]

#sorting names
hhs$name <- NA
#adding names alreadi corrected
for(i in 1 : nrow(hhs)){
  hhs$name[i] <- ifelse(tolower(hhs$hhm_name[i]) %in% census$full_name,
                        tolower(hhs$hhm_name[i]),
                        NA)}
#correcting names from N
for(i in 1 : nrow(hhs)){
  hhs$name[i] <- ifelse( is.na(hhs$name[i]),
                       ifelse(hhs$hhm_name[i] %in% N$old, 
                              N$new[which(N$old == hhs$hhm_name [i])],
                              NA), 
                       hhs$name[i])}
#adding specific names
#replacing 11 individually checked names that were incorrectly spelled with the code
hhs$name[which(hhs$hhm_name == "incorrect_name") ] <- "correct_name"

#adding IDs to hhs
for (i in 1:nrow(hhs)){
  hhs$id[i] <- ifelse(hhs$name[i] %in% census$full_name,
                      census[which (census$full_name == hhs$name[i]), "ID"], 
                      NA)}
#adding specific IDs
#assigning 3 individually checked IDs to individuals with repeated names with the code
hhs$id[which(hhs$hhm_name == "incorrect_id" & hhs$hh_id.x == "household_id")] <- "correct_id"

######################
#FAMILY INFO##########
######################

#create dataframe for family info
family_info <- interviews %>% select(person_id, hh_id)

#SIBSHIP DATA IN RH
#select data
children <- children[ -which(is.na(children$child_name) ),]
children <- children[-which(children$child_name == "cancelled_name"),]

#assign birth order and correct wrong orders
children$birth_order <- children$child_row_number
children$birth_order[is.na(children$birth_order)] <- 13
children$birth_order[which(children$pdf_hash == "6e41a27" & children$birth_order >= 5)] <- children$birth_order[which(children$pdf_hash == "6e41a27" & children$birth_order >= 5)] + 1
children$birth_order[which(children$pdf_hash == "6e41a27" & children$child_row_number == 9)] <- 5
children$birth_order[which(children$pdf_hash == "77d1efa" & children$birth_order >= 4)] <- children$birth_order[which(children$pdf_hash == "77d1efa" & children$birth_order >= 4)] + 1
children$birth_order[which(children$pdf_hash == "77d1efa" & children$child_row_number == 7)] <- 4
children$birth_order[which(children$pdf_hash == "a3ed84a" & children$birth_order >= 3 & children$birth_order <= 6)] <- children$birth_order[which(children$pdf_hash == "a3ed84a" & children$birth_order >= 3 & children$birth_order <= 6)] + 1
children$birth_order[which(children$pdf_hash == "a3ed84a" & children$child_row_number == 6)] <- 3
children$birth_order[which(children$pdf_hash == "eba54d1" & children$child_row_number == 7)] <- 8
children$birth_order[which(children$pdf_hash == "eba54d1" & children$child_row_number == 8)] <- 7

#add children not in reproductive surveys
more_children <- as.data.frame(matrix(NA, nrow = 14, ncol = length(children), dimnames = list (NULL, colnames(children))))
more_children$child_name <- c() #a vector of 14 names not present in the reproductive history (mothers not living in the village)
more_children$birth_order <- c(1, 2, 3, 4, rep(NA, 10)) #assigning birth order for four children who's birth order is known although mother doesn't live in village
children <- rbind(children, more_children)

#sorting names
#correct names
children$name <- NA
#adding names already corrected
for(i in 1 : nrow(children)){
  children$name[i] <- ifelse(tolower(children$child_name[i]) %in% census$full_name,
                        tolower(children$child_name[i]),
                        NA)}
#correct specific names
#replacing 14 individually checked names that were incorrectly spelled with the code
children$name[which(children$child_name == "incorrect name" & children$mother_name == "mother_name")] <- "correct name"

#correct with list of corrections
for(i in 1 : nrow(children)){
  children$name[i] <- ifelse( is.na(children$name[i]),
                              ifelse(children$child_name[i] %in%  N$old,
                                     N$new[which(N$old == children$child_name[i])],
                                     NA),
                              children$name[i])}
#add IDs
for (i in 1:nrow(children)){
  children$id[i] <- ifelse(children$name[i] %in% census$full_name,
                      census[which (census$full_name == children$name[i]), "ID"], 
                      NA)}
#check correct number of children
if(length(children[which(children$id %in% interviews$person_id ), "name"]) != 94 ) warning("Chidren birth order missing _ line 225")
#add birth order info to family info
for( i in 1:nrow(family_info)){
  family_info$birth_order[i] <- children$birth_order[which(children$id == family_info$person_id[i])]
}

#PRESENCE OF ADULTS IN HOUSEHOLD
census$hh_id <- parse_number(census$household_2019, na = c("inferred", "out_of_village", "dead", "REMOVED", NA))
#adding id of parents and indicator of presence
for (i in 1:nrow(family_info)) {
  family_info$mother_id[i] <- census$mother_ID[which(census$ID == family_info$person_id[i])]
  family_info$father_id[i] <- census$father_ID[which(census$ID == family_info$person_id[i])]
  family_info$mother_present[i] <- ifelse(census$hh_id[census$ID == family_info$mother_id[i]] == family_info$hh_id[i], 1, 0)
  family_info$father_present[i] <- ifelse(census$hh_id[census$ID == family_info$father_id[i]] == family_info$hh_id[i], 1, 0)
}
family_info$mother_present[is.na(family_info$mother_present)] <- 0
family_info$father_present[is.na(family_info$father_present)] <- 0

#DISTANCE FROM RESEARCH STATION
for ( i in 1: nrow(family_info)){
  family_info$hh_dist[i] <- h_dist$dist[which(h_dist$HH == family_info$hh_id[i])]
}

######################
#ANONIMIZE DATA#######
######################

interviews$anonyme_id <- anonyme( interviews, "person_id")
freelists$anonyme_id <- anonyme( freelists, "person_id")
ql$anonyme_id <- anonyme( ql, "person_id")
qs$anonyme_id <- anonyme( qs, "person_id")
recognition$anonyme_id <- anonyme( recognition, "person_id")
chores$anonyme_id <- anonyme( chores, "person_id")
hhs$anonyme_id <- anonyme( hhs, "id")
family_info$anonyme_id <- anonyme( family_info, "person_id")

######################
#SELECT RELEVANT DATA#
######################

interviews  <- interviews  %>% select(anonyme_id, hh_id, age, class, sex) %>% arrange(anonyme_id)
freelists   <- freelists   %>% select(anonyme_id, response)  %>% arrange(anonyme_id)
ql          <- ql          %>% select(anonyme_id, response, swahili, english, notes) %>% arrange(anonyme_id)
qs          <- qs          %>% select(anonyme_id, response, swahili, english, notes) %>% arrange(anonyme_id)
recognition <- recognition %>% select(anonyme_id, name_given, picture_id) %>% arrange(anonyme_id)
chores      <- chores      %>% select(anonyme_id, household_help, money_making, seashells, bird_hunting, hunt_with_dogs, field_help, cow_help, fishing, diving ) %>% arrange(anonyme_id)
hh_act      <- hhs         %>% select(anonyme_id, hhm_sex, hhm_age, hhm_class, hhm_household_help,hhm_field_cow,
                                      hhm_fishing_diving,hhm_bird_hunting,hhm_hunt_with_dogs,hhm_money_making,hhm_seashells) %>% arrange(anonyme_id)
family_info <- family_info %>% select(anonyme_id, hh_id, hh_dist, birth_order, mother_present, father_present) %>% arrange(anonyme_id)

######################
#SAVE FILES###########
######################

write.csv(interviews, file =  "2_Data_preparation/anonymized_data/1_interviews.csv", row.names=FALSE)
write.csv(freelists, file = "2_Data_preparation/anonymized_data/2_freelists.csv", row.names=FALSE)
write.csv(ql, file = "2_Data_preparation/anonymized_data/3a_ql.csv", row.names=FALSE)
write.csv(qs, file = "2_Data_preparation/anonymized_data/3b_qs.csv", row.names=FALSE)
write.csv(recognition, file = "2_Data_preparation/anonymized_data/4_recognition.csv", row.names=FALSE)
write.csv(chores, file = "2_Data_preparation/anonymized_data/5_chores.csv", row.names=FALSE)
write.csv(hh_act, "2_Data_preparation/anonymized_data/activities_household.csv", row.names=FALSE)
write.csv(family_info, "2_Data_preparation/anonymized_data/family_info.csv", row.names=FALSE)
