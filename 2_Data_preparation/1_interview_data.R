#Load packages
library(tidyverse)
library(rethinking)
library(here)

setwd(here())

#interviews
interviews <- read.csv("2_Data_preparation/data/1_interviews.csv")
#chores
chores <- read.csv("2_Data_preparation/data/5_chores.csv")
#load freelists
freelists <- read.csv("2_Data_preparation/data/2_freelists.csv")
#questions - long template
ql <- read.csv("2_Data_preparation/data/3a_ql.csv")
#questions - short template 
qs <- read.csv("2_Data_preparation/data/3b_qs.csv")
#picture recognition
recognition <- read.csv("2_Data_preparation/data/4_recognition.csv")

######################
#INDIVIDUALS##########
######################

#check age
interviews$age[which(interviews$age == 1997)] <- 2019 - 1997
interviews$age[which(interviews$age == 1999)] <- 2019 - 1999
interviews$age[which(interviews$age == 12.5)] <- 12
interviews$age[which(interviews$age == 10.5)] <- 10
interviews$age[which(interviews$anonyme_id == "88846")] <- 9 #according to census

#check schooling
interviews$class[which(interviews$class %in% c("CHEK ECHEA","CHEKE CHEA", "CHER ECHEA", "HEKE CHEA"))] <- "CHEKECHEA"
interviews$class[which(interviews$class == "FIN FA")] <- "FIN F1"
interviews$class[which(interviews$class == "FIN")] <- "FIN S6"

#add integer for value of schooling
edu_levels <- data.frame(
  class = sort(unique(interviews$class)),
  n = c( 0 , 1 , 10, 11, 12, 13, 0, 10, 11, 13, 7, 8, 2, 3, 4, 5, 6, 7 ))
for (i in 1:nrow(interviews)) {
  interviews$class_new[i] <- edu_levels$n[which(edu_levels$class == interviews$class[i])]
}

######################
#ACTIVITIES###########
######################

#activities
activities <-  chores[,2:10]
#adds rowname from individual ids
rownames(activities) <- chores$anonyme_id  
#convert tick into binary
activities[activities == "V"] <- 1 #writes 1 where activity is performed
activities[is.na(activities)] <- 0 #otherwise zero
#add columns for activities coded in single column
activities$mwani <- ifelse( grepl( "MW", activities[,2]), 1, 0  ) #adds column for mwani and writes 1 if appears in money making
activities$karafu <- ifelse( grepl( "KA", activities[,2]), 1, 0  )#adds column for karafu and writes 1 if appears in money making
activities <- as.matrix(activities[, -2]) #removes column for money making

#####ATTENTION
#some individuals have missing info on activities
#add NA for people who are missing activities
na.activities <- matrix (NA, nrow = 3 , ncol = length (colnames (activities) ),
                         dimnames = list( interviews[ which (!interviews$anonyme_id  %in% rownames (activities) ), "anonyme_id"],
                                          colnames (activities) ) ) #these people. Need to try to add from hh interviews or add NA
activities <- rbind( activities, na.activities )
#order by ID to match rest of data frames
activities <- activities[order(rownames(activities)), ]



######################
#FREELIST#############
######################

#SPECIAL CORRECTIONS
freelists[which(freelists$response == "KALAMU" &freelists$anonyme_id == "192109" ),"response"] <- "MKALAMU"
freelists[which(freelists$response == "MGULELE" &freelists$anonyme_id == "192431" ),"response"] <- "FUKULILE"
freelists[which(freelists$response == "MITONGO" &freelists$anonyme_id == "137323" ),"response"] <- "TONGO"
freelists[which(freelists$response == "MITONGO" &freelists$anonyme_id == "13212" ),"response"] <- "MTONGA"
freelists[which(freelists$response == "NDEGE" &freelists$anonyme_id == "83881" ),"response"] <- "GEGE"
freelists[which(freelists$response == "MBAAWZI" &freelists$anonyme_id == "69990" ),"response"] <- "PAANZI"

#load corrections freelist
freelist_corrections <- read.csv("2_Data_preparation/data/freelist_corrections.csv")

#correct freelist
for(i in 2 : nrow(freelist_corrections)){
  freelists$response [which(freelists$response == freelist_corrections$old[i])] <- freelist_corrections$new[i]
}


#ADD not_a_creature column
######
not_a_creature <- c("ANDAZI", "ASALI", 
                    "BAHARI", "BAISIKELI", "BAKORA", "BAKULI", "BALBU", "BANDARI", "BANGILI", "BAO", "BARABARA", "BARAZA", "BATI", "BEGI", "BESENI", "BETTRY", "BISCUTI", "BO", "BODABODA", "BOKSI", "BOTI", "BUKTA", "BUKU", "BUSATI",
                    "CAMERA", "CHAI", "CHAJATI", "CHAKULA", "CHANDARUA", "CHAURO", "CHEMSOO", "CHUJIO", "CHUMVI", "CHUPA", 
                    "DAKIKA", "DARAJA BOVU", "DARASA LA KWANZA", "DAU", "DAWA", "DEKI", "DEKSI", "DIRISHA", "DISHI","DISHI LA UMEME", "DULA LA SHINDANO", "DUMU",
                    "FAGIO", "FAIBA", "FENI", "FINISH", "FIRIMBI MDOMO", "FLASHI", "FONDESHENI", "FONI", "FORONYA", "FREMU", "FULANA", "FUMBUKA", 
                    "GAMBA", "GARI", "GATI", "GLASI", "GODORO", "GOME", "GOME LA MTI", "GONGO", "GUNDI",
                    "HAMIRA", "HARUSI", "HELICOPTER", "HELMETI", "HERINI", 
                    "JAGI", "JAHAZI", "JARIFE", "JEMBE", "JIKO", "JIKO LA UMEME", "JINI", "JIWE", "JUISI", 
                    "KABATI", "KAHAWIA", "KALAMU", "KANZU", "KARAI", "KARATASI", "KATA", "KAURE", "KAVA", "KIBANDA", "KIBAPA", "KIBATI CHA DAWA", "KICHUPA CHA PODA", "KICHWA", "KIDEVU", "KIDOA", "KIDOO", "KIDUDE CHA KUPIMIA MCHELE", "KIFUA", "KIFUKO", "KIFUNGO", "KIFUNIKIO", "KIFUU", "KIHANJIFU", "KIJIKO", "KIJUZUU", "KIKOBA CHA NYUMA", "KILEMBA", "KIPANDE CHA MNAZI", "KIPIMO", "KIREMBO", "KIROHO", "KISADO", "KISAGIO", "KISIMA", "KISIWA", "KISUGUDI", "KITABU", "KITAMBAA", "KITANDA", "KITI", "KIUNGO", "KIUNGO CHA NGUU", "KIUNO", "KIWEMBE", "KIWI", "KIZIO", "KOFIA", "KOKOCHI", "KOKOTO", "KOKWA ZA FENESI", "KOMA", "KONDE", "KOPA", "KOPE", "KORIDO", "KOROBWE", "KOROGWE", "KOROMA", "KOTI", "KOYA", "KU PAKUA", "KUFULI", "KULA", "KUMVI", "KUNI", "KUPIMA", "KUSUUNZA", "KUTAMBA", 
                    "LEGEZA",
                    "MACHO", "MADE YA VITANDA", "MADIRA", "MAEPO", "MAFYA", "MAGLAVU", "MAGUBI", "MAJI", "MAJIMAJI", "MAKARARA", "MAKOPO", "MAKOSA", "MAKOSA KUUGWA", "MAKUMBI", "MAKUTI", "MANJANO", "MAPELE", "MASHUA", "MASIKIO", "MATAKO", "MATUTA", "MAVA", "MAVIMAVI", "MAZIWA", "MBEGU", "MBOGA", "MBOGABOGA", "MCHANGA", "MCHOKO", "MCHUZI", "MDOMO", "MELI", "MENO", "MESA", "MFEREJI", "MFUKO", "MFUNIKIO", "MFUNIKIO WA SIMU", "MFUPA WA KUKU", "MFUPI", "MGELEMA", "MGOGONI", "MGUU", "MHIMBILI", "MICHEWENI", "MIKO", "MITA", "MITI YA SHAMBA", "MKAA", "MKIA WA NGOMBE", "MKOANI", "MKONO", "MKONO WA SAA", "MKUFU", "MLANGO", "MNARANI", "MOTO", "MPINI", "MPIRA", "MSAAFU", "MSHIPI", "MSIKITI", "MSINGI", "MSITU", "MSKITINI", "MSTARI", "MSUAKI", "MSUKA", "MSUMENO", "MTANDIO", "MTI", "MTO", "MTOTO", "MTUHALIWA", "MTUMBWI", "MUWASHO", "MWALIMU", "MWIBA", "MWIKO", "MZIGO",  
                    "NDEGE", "NDOANA", "NDOO", "NEMBO", "NGARAWA", "NGUMU", "NGUO", "NJANO", "NJUKUTI", "NOHA", "NSEME", "NYAMA", "NYAVU", "NYAYO", "NYOTA", "NYUMBA", "NYUNDO", "NYUSI", "NYWELE",
                    "PAIPU", "PAJA", "PAURO", "PEMPAS", "PENI", "PENSELI", "PENSI", "PESA", "PETE", "PEZI", "PICHA", "PIKIPIKI", "POLO", "POVU", "PUA", "PUKUSA", "PWANI", 
                    "RANGI", "RANGI MBILI", "RAYANI", "REDIO", "REZA", "RINGA", 
                    "SAMAKI", "SAA", "SABUFA", "SABUNI", "SABUNI ZA TUMBILI", "SAHANI", "SAMLI", "SARUJI", "SEKUNDE", "SERUNI", "SHANGA", "SHANUO", "SHATI", "SHEITANI", "SHIMO", "SHIMO LA JONGOO", "SHINA LA MBIRIMBI", "SHINDANO", "SHINGO", "SHUKA", "SHUNGI", "SIAGI", "SIMU", "SINGBODI", "SINGILENDI", "SIPIKA", "SKETI", "SOLAR", "SUFURIA", "SUKARI", "SUMAKU", "SUNGUSUNGU", "SURUALI", "SUSA",
                    "TANGI", "TANGO", "TIME", "TITI", "TORCHI", "TUFALI", "TUI", "TV", "UBONGO", "UCHAFU", "UFUNGUO", "UKUTA", "UMEME", "UNGUJA", "URIMBO", "UZI", 
                    "VESPA", "VIATU", "VIBATALI", "VICHANGA", "VIDEO", "VIDOLE", "VIFIRIMBI", "VIKAO", "VINU", "VIPEZI", "VOMBO", "VOUCHER", "VYOMBO",
                    "WAGE", "WALETI", "WALI", "WANJA", "WATOTO PACHA", "WATOTO WANAUME PACHA", "WETE", "WIRE", 
                    "YAI", 
                    "ZIPU")

#####  

#add not_a_creature column
freelists$not_a_creature <- ifelse( freelists$response  %in% not_a_creature, 1, 0)

#subset freelist to include only creatures
freelists <- freelists[which (freelists$not_a_creature == 0),] 

#check results
all_items <- freelists %>% group_by(response) %>% count()
all_items <- all_items[-1,]

#PREPARE DATA
#matrix of answers
Y_l <- matrix(data = NA, nrow = length(interviews$anonyme_id) , ncol = length(all_items$response), dimnames = list( interviews$anonyme_id, all_items$response))
for (i in 1:length(interviews$anonyme_id)) {
  for(j in 1:length(all_items$response)){
    Y_l[i,j] <- ifelse(nrow(freelists[ #if at least a row where individual i names object j exist, write 1
      which(freelists$anonyme_id == rownames(Y_l)[i] 
            & freelists$response == colnames(Y_l)[j] ),]) >= 1 , 1, 0)
  }
}

######################
#QUESTIONNAIRE########
###################### 

#adding questions stored in notes
#manually list questions and answers
######
extra_qns <- list(
  kamba_a = c("KAMBA (A)", "KAMBA A", "KAMBAA", "KAMSA A", "MAFUTA / KAMDA A"),
  kamba_b = c("KAMBA B"),
  kutambaa_a = c("KUTAMBAA A"),
  kutambaa_b = c("KUTAMBAA B", "KUTAMBAA B PARABE"),
  kutambaa_nk= c("KUTAMBAA BOTH", "KUTAMBAA DONT KNOW", "KUTAMBAA NONE"),
  mikandaa_a = c("MIKANDAA A"),
  mikandaa_b = c("MIK. B", "MIKANDA B","MIKANDAA B","MIKANDAA B DONT KNOW","MIKANOAA B", "TOONDO B"),
  mikandaa_nk= c("MIKANDAA BOTH","MIKANDAA DONT KNOW","MIKANDAA NONE"),
  ngisi_a = c("BOTH NGIJI A","BOTH NGIZI A","NGIJI A", "NGISI A", "WGISI AA"),
  ngisi_b = c("NGISI B", "NGIZI B"),
  chowe_a = c("UFUKWENI A"),
  chowe_b = c("UFUKWENI B", "VFUKWENI B"),
  chowe_nk = c("UFUFWENI BOTH", "UFUKWENI BOTH", "UFUKWENI Dont know", "UFUKWENI DONT KNOW", "UFUKWENI NOT KNOW"),
  vijinu_a = c("UIJINU A", "UIJINV A", "VIJINU A", "VIJINU"),
  vijinu_b = c("UIJINU B", "VIJINU B"),
  vijinu_nk = c("UIJINU DONTKNN", "VIJINU DONT KNOW", "VIJINU NOT KNOW",  "VIJINV DONTKWN"))

extra_answers <- list(
  swahili = c( "kamba","kamba",
               "kutambaa","kutambaa","kutambaa",
               "mikandaa","mikandaa","mikandaa",
               "ngisi","ngisi",
               "chowe","chowe","chowe",
               "vijinu","vijinu","vijinu"),
  english = c( "shrimps","shrimps",
               "vine","vine","vine",
               "mangroves","mangroves","mangroves",
               "squid","squid",
               "shells","shells","shells",
               "cone_shells","cone_shells","cone_shells"),
  response = c( "a", "b",
                "a", "b", "not_known",
                "a", "b", "not_known",
                "a", "b",
                "a", "b", "not_known",
                "a", "b", "not_known")
)
#####

for (i in 1:length(extra_qns)) {
  new_rows <- qs[which(qs$notes %in% extra_qns[[i]]),]
  new_rows$swahili <- extra_answers$swahili[i]
  new_rows$english <- extra_answers$english[i]
  new_rows$response <- extra_answers$response[i]
  qs <- rbind(qs, new_rows)
}

#bind dataframes
questionnaire <- rbind(ql, qs)

#correct questions
questionnaire$swahili[which(questionnaire$english == "sharks_offspring")] <- "papa_watoto"
questionnaire$swahili[which(questionnaire$english == "sharks_dangerous")] <- "papa_mkali"

#load correct answers
correct_answers <- read.csv("2_Data_preparation/data/questions_correct_answers.csv")
correct_answers <- correct_answers %>%  select(QN.number, Right.answer ) %>% 
  rename( swahili = QN.number, right_answer = Right.answer) 
correct_answers$right_answer <- tolower(correct_answers$right_answer)

#create matrix
Y_q <- matrix(data = NA, nrow = length(interviews$anonyme_id) , ncol = length(unique(questionnaire$swahili)), dimnames = list( interviews$anonyme_id, unique(questionnaire$swahili)))
for (i in 1:length(interviews$anonyme_id)) {
  for(j in 1:length(unique(questionnaire$swahili))){
    Y_q[i,j] <- nrow(questionnaire[ #per each individual and question, if the answer given is correct, the datafame will be restricted to one row - giving 1 in the matrix. Check that there is not
      which(questionnaire$anonyme_id == rownames(Y_q)[i] #finds individual ID
            & questionnaire$swahili == colnames(Y_q)[j] #finds question ID
            & questionnaire$response == correct_answers[which(correct_answers$swahili == colnames(Y_q)[j]), "right_answer"]),]) #checks if answer is correct
  }
  
}


######################
#PICTURE RECOGNITION##
######################

#recognition$picture_n <- sub("\\_.*", "", recognition$picture_id)

#corrections
#####
recognition$name_given[which(recognition$name_given == "-")] <- "NA"
recognition$name_given[which(recognition$name_given == "*")] <- "NA"
recognition$name_given[which(recognition$name_given == "I")] <- "NA"
recognition$name_given[which(recognition$name_given == "M")] <- "NA"
recognition$name_given[which(recognition$name_given == "MLIHOGO")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MLIMAU MKUNDU")] <- "MLIMAU"
recognition$name_given[which(recognition$name_given == "WANGA-MCHOCHONI")] <- "MCHOCHONI"
recognition$name_given[which(recognition$name_given == "MNAZI; MVUNYA")] <- "MNAZI" #img20
recognition$name_given[which(recognition$name_given == "MCHOCHA; MLIMAU")] <- "MLIMAU" #img20
recognition$name_given[which(recognition$name_given == "MAMKUNGU; MIGOMBA")] <- "MIGOMBA" #img20
recognition$name_given[which(recognition$name_given == "MNAZI + MPAPINDI")] <- "MNAZI"#img20
recognition$name_given[which(recognition$name_given == "MUHOGO + MWEMBE")] <- "MUHOGO"#img20
recognition$name_given[which(recognition$name_given == "MSHELI + MGOMBA")] <- "MGOMBA"#img20
recognition$name_given[which(recognition$name_given == "MSTAFELI" & recognition$anonyme_id == "12753")] <- "MWEMBE"#img20
recognition$name_given[which(recognition$name_given == "JONGOO LePWANI")] <- "JONGOO LA PWANI"
recognition$name_given[which(recognition$name_given == "MIGOMBA + MTUTUTU")] <- "MIGOMBA"#img20
recognition$name_given[which(recognition$name_given == "MICHEKUNDU; MSISIMIZI")] <- "MSISIMIZI"
recognition$name_given[which(recognition$name_given == "MCHIKICHI; MNAZI")] <- "MNAZI"
recognition$name_given[which(recognition$name_given == "UITUGUU")] <- "VITUGUU"
recognition$name_given[which(recognition$name_given == "MWEMBE; MVINJE")] <- "MWEMBE"#img20
recognition$name_given[which(recognition$name_given == "MCHO WOLMI")] <- "MVINJE"#img20
recognition$name_given[which(recognition$name_given == "MCHOCHA MLIMAU")] <- "MLIMAU"#img20
recognition$name_given[which(recognition$name_given == "MSHELISHELI"& recognition$anonyme_id == "193223")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MIEMBE; MZAMBARAU"& recognition$anonyme_id == "193223")] <- "MIEMBE"
recognition$name_given[which(recognition$name_given == "MIVINJE; MUHOGO"& recognition$anonyme_id == "193223")] <- "MIVINJE"
recognition$name_given[which(recognition$name_given == "MCHUNGWA; MVINJE")] <- "MVINJE"#img20
recognition$name_given[which(recognition$name_given == "MWEMBE + MCHOCHA")] <- "MWEMBE"#img20
recognition$name_given[which(recognition$name_given == "MKUNGU +")] <- "MKUNGU"
recognition$name_given[which(recognition$name_given == "MISISIMIZI; MIPUNGA")] <- "MISISIMIZI"
recognition$name_given[which(recognition$name_given == "MIEMBE; MCHUNGWA")] <- "MWEMBE"#img20
recognition$name_given[which(recognition$name_given == "MCHIKICHI; MBILINGANI")] <- "MLIMAU"#img20
recognition$name_given[which(recognition$name_given == "DAKTARI YA NGOMBE; DAKTARI YA NGOMBE")] <- "DAKTARI YA NGOMBE"
recognition$name_given[which(recognition$name_given == "MCHOCHA-MWEMBE")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "SISIMIZI; MPERA")] <- "MSISIMIZI"
recognition$name_given[which(recognition$name_given == "CHATU GANGAWIA UKUKWI")] <- "UKUKWI"
recognition$name_given[which(recognition$name_given == "GANGAWIA-UKUKWI")] <- "UKUKWI"
recognition$name_given[which(recognition$name_given == "NYOKA-UKUKWI")] <- "UKUKWI"
recognition$name_given[which(recognition$name_given == "KOJOKOJO-SISIMIZI")] <- "KOJOKOJO"
recognition$name_given[which(recognition$name_given == "KUNGURU; KUNGORU")] <- "KUNGURU"
recognition$name_given[which(recognition$name_given == "MKANDAA + MBUNGO")] <- "MBUNGO"
recognition$name_given[which(recognition$name_given == "TUMBILI +")] <- "TUMBILI"
recognition$name_given[which(recognition$name_given == "MCHIKICHI MNAZI")] <- "MNAZI"
recognition$name_given[which(recognition$name_given == "MCHIKICHI + MINAZI")] <- "MNAZI"
recognition$name_given[which(recognition$name_given == "MINAZI; MCHIIKICHI")] <- "MNAZI"
recognition$name_given[which(recognition$name_given == "MCHIKICHI; MINAZI")] <- "MNAZI"
recognition$name_given[which(recognition$name_given == "MCHIKICHI - MINAZI")] <- "MINAZI"
recognition$name_given[which(recognition$name_given == "MUHOGO-MCHOCHA")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MUHOGO; MKUNGU")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "VISIKI MSASA")] <- "VISIKI"
recognition$name_given[which(recognition$name_given == "MUHOKO (VISIKI YA)")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MUHOGO MKUNGU")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MUHOGO - MZAMBARAU")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MISIKI; MBEGU")] <- "VISIKI"
recognition$name_given[which(recognition$name_given == "VISIKI + MGOMBA")] <- "VISIKI"
recognition$name_given[which(recognition$name_given == "VISIKHMUHOGO")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MUHOGO + CHANGA + MIRIBA")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MUHOGO + VIAZI")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MIGOMBA +")] <- "MIGOMBA"
recognition$name_given[which(recognition$name_given == "MWEMBE -")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MVINJE; MWEMBE")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MPERA"& recognition$anonyme_id == "18758")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MCHOCHO + MKUNGU")] <- "MKUNGU"
recognition$name_given[which(recognition$name_given == "MVINJE; STAFELI")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MVINJE-MKUNGO")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MIGOMBA-MSISIMIZI")] <- "MIGOMBA"
recognition$name_given[which(recognition$name_given == "MUHOGO; MKUNGU")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MCHIKICHI; MWEMBE")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MVINJE; MSHELI")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MIEMBE; MPENDAPENDAPO")] <- "MIEMBE"
recognition$name_given[which(recognition$name_given == "MWEMBE + MSTAFELI")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MLIMAU + MKUNGU")] <- "MLIMAU"
recognition$name_given[which(recognition$name_given == "MVINJE; MPERA; MGOMBA")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MVINJE; MKUNGU")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MPERA + MWEMBE")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MVINJE; MIBONO")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MICHUNGWA"& recognition$anonyme_id == "138451")] <- "MBONO"#img20
recognition$name_given[which(recognition$name_given == "MSTAFELI"& recognition$anonyme_id == "147436")] <- "MKUNGU"#img20
recognition$name_given[which(recognition$name_given == "MUHOGO MKUNGU")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MVINJE; MUUNYA")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MIEMBE; MIZAMBARAU")] <- "MIEMBE"
recognition$name_given[which(recognition$name_given == "MUINJE MGOMBA")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MSHELI"& recognition$anonyme_id == "122557")] <- "MGOMBA"#img20
recognition$name_given[which(recognition$name_given == "MCHIKICHI + MWEMBE")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MVINJE + MCHOCHA")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "VISIKI + MGOMBA")] <- "VISIKI"
recognition$name_given[which(recognition$name_given == "MISHELI"& recognition$anonyme_id == "81873")] <- "MUHOGO"
recognition$name_given[which(recognition$name_given == "MINAZI + MPERA + MVINJE")] <- "MINAZI"
recognition$name_given[which(recognition$name_given == "MWEMBE + MGOMBA")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MDIMU + MKUNGU")] <- "MKUNGU"
recognition$name_given[which(recognition$name_given == "MCHUCHA + MVUMANYTI")] <- "MGOMBA"
recognition$name_given[which(recognition$name_given == "MVINJE - MWEMBE")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MISASA - MKUNGU")] <- "MKUNGU"
recognition$name_given[which(recognition$name_given == "MCHUNGWA"& recognition$anonyme_id == "14109")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MWEMBE + MVINJE")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MZAMBARAU"& recognition$anonyme_id == "132853")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MCHOCHA + MIMUNYA")] <- "MVINJE"
recognition$name_given[which(recognition$name_given == "MCHUNGWA + MKUNGU")] <- "MKUNGU"
recognition$name_given[which(recognition$name_given == "MCHUNGWA. MWEMBE")] <- "MWEMBE"
recognition$name_given[which(recognition$name_given == "MIPERA MHRA")] <- "MIPERA"
recognition$name_given[which(recognition$name_given == "NYASI; SISIMIZI")] <- "SISIMIZI"
recognition$name_given[which(recognition$name_given == "SISIMIZI MFUU")] <- "SISIMIZI"
recognition$name_given[which(recognition$name_given == "MPUNGA"& recognition$anonyme_id == "14109")] <- "MPERA"
recognition$name_given[which(recognition$name_given == "BOCHO TOTOVU")] <- "TOTOVU"
recognition$name_given[which(recognition$name_given == "VIPEPERO - PUJU")] <- "KIPEPEO"
recognition$name_given[which(recognition$name_given == "MWANI + MACHEME")] <- "MACHEME"
recognition$name_given[which(recognition$name_given == "KIJUNGU MAFUTA; MDODOSI")] <- "KIJUNGU MAFUTA"
recognition$name_given[which(recognition$name_given == "PUJU; KIJUNGU MAFUTA")] <- "KIJUNGU MAFUTA"
recognition$name_given[which(recognition$name_given == "KISANGE + KIJUNGU MAFUTA")] <- "KIJUNGU MAFUTA"
recognition$name_given[which(recognition$name_given == "MACHAME MALAINI")] <- "MACHAME"
recognition$name_given[which(recognition$name_given == "MACHEME, JIWE")] <- "MACHEME"
recognition$name_given[which(recognition$name_given == "MACHEME MAJIWE")] <- "MACHEME"
recognition$name_given[which(recognition$name_given == "MAWE + MACHEME")] <- "MACHEME"
recognition$name_given[which(recognition$picture_id == "26_b_shell"& recognition$anonyme_id == "130099")] <- "MWAMIZE"#written in 26_c
recognition$name_given[which(recognition$picture_id == "27_a_cucumber"& recognition$anonyme_id == "252552")] <- "jongoo la pwani"#written in 26_c
recognition$name_given[which(recognition$name_given == "JONGOO WA PWANI (or KIFUZA)")] <- "JONGOO LA PWANI"
recognition$name_given[which(recognition$name_given == "WANGA MCHOCHUM")] <- "MCHOCHONI"
recognition$name_given[which(recognition$name_given == "WANGA + CHOCHONI")] <- "MCHOCHONI"
recognition$name_given[which(recognition$name_given == "MCHOKICHOKe")] <- "MCHOKICHOKI"
recognition$name_given[which(recognition$name_given == "MCHANJA-MKANDAA")] <- "MKANDAA"
recognition$name_given[which(recognition$picture_id == "13_a_bird"& recognition$anonyme_id == "82623")] <- "KONGO MAJOKA"#written in 12_b
recognition$name_given[which(recognition$picture_id == "13_b_tree"& recognition$anonyme_id == "82623")] <- "MKUNGU"#written in 13_a
recognition$name_given[which(recognition$picture_id == "13_b_tree"& recognition$anonyme_id == "61153")] <- "MKUNGU"#written in 13_a
#####

#recognition[which(recognition$name_given == "MIPERA MHRA"),]

#CORRECT ANSWERS
#load correct answers
image_answers <- read.csv("2_Data_preparation/data/recognition_correct_answers.csv")

#create vector with all possible answers to image recognition
image_answers$all_answ <- ifelse(is.na(image_answers$OK), image_answers$s, paste(image_answers$s,image_answers$OK, sep = ","))#pastes together all possible answers
image_answers$all_answ <- toupper(image_answers$all_answ)
image_answers$all_answ <- strsplit(image_answers$all_answ, "[,]")
#create item id
image_answers$picture_id <- paste(image_answers$ID.image, image_answers$Item , image_answers$organism, sep = "_")#creates creature id

#create variable of recognition - if answer is present in list of possible answers
for(i in 1: nrow(recognition)){
  recognition$recognized[i] <- ifelse ( 
    recognition$name_given[i] %in% image_answers [[which( #if the name given by the interviewee appears
      image_answers$picture_id==recognition$picture_id[i]), "all_answ"]],#among the possible answes
    1,0)#it returns 1 on the "recognized' column, otherwise 0
}

#remove impossible task - leaves in picture 3
recognition <- recognition[-which(recognition$picture_id == "3_c_plant_leav"),]

#create matrix of answers
Y_r <- matrix(data = NA, nrow = length(interviews$anonyme_id) , ncol = length(unique(recognition$picture_id)), dimnames = list( interviews$anonyme_id, unique(recognition$picture_id)))
for (i in 1:length(interviews$anonyme_id)) {
  for(j in 1:length(unique(recognition$picture_id))){
    Y_r[i,j] <- recognition[which(recognition$anonyme_id == rownames(Y_r)[i] #per each individual
                                  & recognition$picture_id == colnames(Y_r)[j]), #and item
                            "recognized"]#it enters whether the item is recognized or not
  }#j
}#i


######################
#PREPARE DATA#########
######################
#create data list
d <- list( N = nrow(interviews),                         #n individuals
           H = length(unique(interviews$hh_id)),         #n households
           HH= interviews$hh_id,                         #integer for household
           A = standardize(as.numeric(interviews$age)) , #standardized age
           SY= standardize(interviews$class_new),        #standardized n of years of school
           am= activities,                               #activities practiced
           C = ncol(activities),                         #n of activities
           L = ncol(Y_l),                                #n of items in freelist
           Q = ncol(Y_q),                                #n of items in questions
           R = ncol(Y_r),                                #n of items in picture recognition
           Y_l = Y_l[order(rownames(Y_l)), ],            #matrix of answers for freelist
           Y_q = Y_q[order(rownames(Y_q)), ],            #matrix of answers for questions
           Y_r = Y_r[order(rownames(Y_r)), ])            #matrix of answers for picture recognition

write.csv(d , "2_Data_preparation/data.csv")

rm( all_items, 
    activities, 
    census,
    correct_answers, 
    chores,  
    edu_levels,
    extra_answers, 
    extra_qns, 
    freelists, 
    freelist_corrections,
    image_answers,
    interviews,
    new_rows,
    not_a_creature,
    ql, qs,
    questionnaire,
    recognition,
    Y_l, Y_q, Y_r)


