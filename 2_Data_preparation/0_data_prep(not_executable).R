#the following script is not executable, included only for transparency

#Load packages
library(tidyverse)
library(here)


#loads functions
if( !exists( "ID_check", mode = "function")) source(paste ( here(), "2_Data_preparation/functions.r", sep = "/") )

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

######################
#ANONIMIZE DATA#######
######################

#original IDs include 5 letters and 4 random numbers. 
#One or two digits corresponding to the first letter are added to the random digits to create unique anonymous IDs
interviews$anonyme_id <- anonyme( interviews, "person_id")
freelists$anonyme_id <- anonyme( freelists, "person_id")
ql$anonyme_id <- anonyme( ql, "person_id")
qs$anonyme_id <- anonyme( qs, "person_id")
recognition$anonyme_id <- anonyme( recognition, "person_id")
chores$anonyme_id <- anonyme( chores, "person_id")

######################
#SELECT RELEVANT DATA#
######################

interviews  <- interviews  %>% select(anonyme_id, hh_id, age, class) %>% arrange(anonyme_id)
freelists   <- freelists   %>% select(anonyme_id, response)  %>% arrange(anonyme_id)
ql          <- ql          %>% select(anonyme_id, response, swahili, english, notes) %>% arrange(anonyme_id)
qs          <- qs          %>% select(anonyme_id, response, swahili, english, notes) %>% arrange(anonyme_id)
recognition <- recognition %>% select(anonyme_id, name_given, picture_id) %>% arrange(anonyme_id)
chores      <- chores      %>% select(anonyme_id, household_help, money_making, seashells, bird_hunting, hunt_with_dogs, field_help, cow_help, fishing, diving ) %>% arrange(anonyme_id)

######################
#SAVE FILES###########
######################

write.csv(interviews, file = paste ( here(), "2_Data_preparation/data/1_interviews.csv", sep = "/"), row.names=FALSE)
write.csv(freelists, file = paste ( here(), "2_Data_preparation/data/2_freelists.csv", sep = "/"), row.names=FALSE)
write.csv(ql, file = paste ( here(), "2_Data_preparation/data/3a_ql.csv", sep = "/"), row.names=FALSE)
write.csv(qs, file = paste ( here(), "2_Data_preparation/data/3b_qs.csv", sep = "/"), row.names=FALSE)
write.csv(recognition, file = paste ( here(), "2_Data_preparation/data/4_recognition.csv", sep = "/"), row.names=FALSE)
write.csv(chores, file = paste ( here(), "2_Data_preparation/data/5_chores.csv", sep = "/"), row.names=FALSE)
