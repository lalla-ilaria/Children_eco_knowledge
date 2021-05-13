#Load packages
library(tidyverse)
library(rethinking)
library(rlist)

#setwd("../")

#interviews
interviews <- read.csv("2_Data_preparation/anonymized_data/1_interviews.csv")
#chores
chores <- read.csv("2_Data_preparation/anonymized_data/5_chores.csv")
#load freelists
freelists <- read.csv("2_Data_preparation/anonymized_data/2_freelists.csv")
#questions - long template
ql <- read.csv("2_Data_preparation/anonymized_data/3a_ql.csv")
#questions - short template 
qs <- read.csv("2_Data_preparation/anonymized_data/3b_qs.csv")
#picture recognition
recognition <- read.csv("2_Data_preparation/anonymized_data/4_recognition.csv")

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

#added reviewed ages after cross control with multiple data
ages <- data.frame( anonyme_id = c(12168,15888,236324,112462,138451,11879,233890,193223,61212,138891,
                                   1949,201358,19586,185824,112713,188722,83881,11589,17547,18749,
                                   252539,262057,154176,195755,195424,9108,135723,195721,18758,61427,
                                   83476,194104,186802,195821,37559,18232,138251,132689,253748,14109,
                                   267566,185445,14764,133433,1399,199586,147436,65243,197299,194388,
                                   192966,192662,184723,13744,122557,192109,18959,152122,137323,132588,
                                   11115,132853,12165,12185,83275,61153,13212,198846,88846,82613,
                                   82623,136745,62866,191985,251533,81763,81873,133453,153562,192431,
                                   263632,63263,12761,252611,12753,252552,193755,199450,29434,132854,
                                   147833,195799,62988,69990),
                    summary_age = c(13,6,6,17,22,20,12,9,12,15,
                                    17,19,56,6,8,9,11,14,7,9,
                                    7,12,14,17,19,9,5,12,13,16,
                                    18,7,11,14,16,7,9,13,6,12,
                                    7,8,11,13,13,16,19,10,15,16,
                                    24,12,11,9,12,14,14,18,5,7,
                                    11,13,7,12,12,14,15,11,9,10,
                                    13,26,15,17,7,13,13,10,11,13,
                                    11,13,22,8,11,12,15,17,20,21,
                                    13,14,13,12),
                    decision = c("coherent","amina","amina","amina","themselves","themselves","coherent","cheti","cheti","cheti",
                               "cheti","amina+coherent","themselves","cheti","coherent","cheti","both amina and school","both amina and school","school","school",
                               "cheti","cheti","cheti","amina","coherent","cheti","amina","school","school","coherent",
                               "school","amina","both amina and school","amina","amina","coherent","coherent","themselves","school","school",
                               "interview","interview","coherent","school","school","themselves","themselves","school","coherent","themselves",
                               "themselves","school","school",NA,"school","themselves","coherent","coherent","coherent","themselves",
                               "coherent","coherent","amina","coherent","amina","themselves",NA,"coherent","coherent","coherent",
                               "school","themselves","amina","amina","school","amina+school","amina+school","school","themselves","themselves",
                               "school","school","themselves","themselves","coherent","all amina and cheti","school","coherent","school","coherent",
                               "coherent","school","coherent","amina"))


interviews  <- merge( interviews, ages, by = "anonyme_id")

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
#add values sourced from the household interviews for people who are missing activities in the knowledge interviews
na.activities <- matrix (NA, nrow = 3 , ncol = length (colnames (activities) ),
                         dimnames = list( interviews[ which (!interviews$anonyme_id  %in% rownames (activities) ), "anonyme_id"],
                                          colnames (activities) ) ) #these people. Need to try to add from hh interviews or add NA
na.activities[ which(rownames(na.activities) == 132588),] <- c (0,0,1,0,0,0,0,0,0,1)
na.activities[ which(rownames(na.activities) == 136745),] <- c (0,0,0,1,1,0,1,1,1,1)
na.activities[ which(rownames(na.activities) == 236324),] <- c (1,1,0,0,0,0,0,0,0,0)




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
freelist_corrections <- read.csv("2_Data_preparation/anonymized_data/freelist_corrections.csv")

#correct freelist
for(i in 2 : nrow(freelist_corrections)){
  freelists$response [which(freelists$response == freelist_corrections$old[i])] <- freelist_corrections$new[i]
}


#ADD not_a_creature column
######
not_a_creature <- c("ALI", "ANDAZI", "ASALI", 
                    "BAHARI", "BAISIKELI", "BAKORA", "BAKULI", "BALBU", "BANDARI", "BANGILI", "BAO", "BARABARA", "BARAZA", "BATI", "BEGI", "BESENI", "BETTRY", "BISCUTI", "BO", "BODABODA", "BOKSI", "BOTI", "BUKTA", "BUSATI",
                    "CAMERA", "CHAI", "CHAJATI", "CHAKULA", "CHANDARUA", "CHAURO", "CHEMSOO", "CHUJIO", "CHUMVI", "CHUPA", 
                    "DAKIKA", "DARAJA BOVU", "DARASA LA KWANZA", "DAU", "DAWA", "DEKI", "DEKSI", "DIRISHA", "DISHI","DISHI LA UMEME", "DULA LA SHINDANO", "DUMU",
                    "FAGIO", "FAIBA", "FENI", "FINISH", "FIRIMBI MDOMO", "FLASHI", "FONDESHENI", "FONI", "FORONYA", "FREMU", "FULANA", "FUMBUKA", 
                    "GAMBA", "GARI", "GATI", "GLASI", "GODORO", "GOME", "GOME LA MTI", "GONGO", "GUNDI",
                    "HAMIRA", "HARUSI", "HELICOPTER", "HELMETI", "HERINI", 
                    "JAGI", "JAHAZI", "JARIFE", "JEMBE", "JIKO", "JIKO LA UMEME", "JINI", "JITI", "JIWE", "JUISI", 
                    "KABATI", "KAHAWIA", "KALAMU", "KANZU", "KARAI", "KARATASI", "KATA", "KAURE", "KAVA", "KIBANDA", "KIBAPA", "KIBATI CHA DAWA", "KICHUPA CHA PODA", "KICHWA", "KIDEVU", "KIDOA", "KIDOO", "KIDUDE CHA KUPIMIA MCHELE", "KIFUA", "KIFUKO", "KIFUNGO", "KIFUNIKIO", "KIFUU", "KIHANJIFU", "KIJIKO", "KIJUZUU", "KIKOBA CHA NYUMA", "KILEMBA", "KIPANDE CHA MNAZI", "KIPIMO", "KIREMBO", "KIROHO", "KISADO", "KISAGIO", "KISIMA", "KISIWA", "KISUGUDI", "KITABU", "KITAMBAA", "KITANDA", "KITI", "KIUNGO", "KIUNGO CHA NGUU", "KIUNO", "KIWEMBE", "KIWI", "KIZIO", "KOFIA", "KOKOCHI", "KOKOTO", "KOKWA ZA FENESI", "KOMA", "KONDE", "KOPA", "KOPE", "KORIDO", "KOROGWE", "KOROMA", "KOTI", "KOYA", "KU PAKUA", "KUFULI", "KULA", "KUMVI", "KUNI", "KUPIMA", "KUSUUNZA", "KUTAMBA", 
                    "LEGEZA",
                    "MACHO", "MADE YA VITANDA", "MADIRA", "MAEPO", "MAFYA", "MAGLAVU", "MAGUBI", "MAJI", "MAJIMAJI", "MAKARARA", "MAKOPO", "MAKOSA", "MAKOSA KUUGWA", "MAKUMBI", "MAKUTI", "MAJANI", "MANJANO", "MAPELE", "MASHUA", "MASIKIO", "MATAKO", "MATUTA", "MAVA", "MAVIMAVI", "MAZIWA", "MBEGU", "MBOGA", "MBOGABOGA", "MCHANGA", "MCHOKO", "MCHUZI", "MDOMO", "MELI", "MENO", "MESA", "MFEREJI", "MFUKO", "MFUNIKIO", "MFUNIKIO WA SIMU", "MFUPA WA KUKU", "MFUPI", "MGELEMA", "MGOGONI", "MGUU", "MHIMBILI", "MICHEWENI", "MIKO", "MITA", "MITI YA SHAMBA", "MKAA", "MKIA WA NGOMBE", "MKOANI", "MKONO", "MKONO WA SAA", "MKUFU", "MLANGO", "MMEA", "MNARANI", "MOTO", "MPINI", "MPIRA", "MSAAFU", "MSHIPI", "MSIKITI", "MSINGI", "MSITU", "MSKITINI", "MSTARI", "MSUAKI", "MSUKA", "MSUMENO", "MTANDIO", "MTI", "MTO", "MTOTO", "MTUHALIWA", "MTUMBWI", "MUWASHO", "MWALIMU", "MWIBA", "MWIKO", "MWITU", "MZIGO",  
                    "NDEGE", "NDOANA", "NDOO", "NEMBO", "NGARAWA", "NGUMU", "NGUO", "NJANO", "NJUKUTI", "NOHA", "NSEME", "NYAMA", "NYAVU", "NYAYO", "NYOTA", "NYUMBA", "NYUNDO", "NYUSI", "NYWELE",
                    "PAIPU", "PAJA", "PAURO", "PEMPAS", "PENI", "PENSELI", "PENSI", "PESA", "PETE", "PEZI", "PICHA", "PIKIPIKI", "PIPI", "POLO", "POVU", "PUA", "PUKUSA", "PWANI", 
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


#add type of item
#################
#list all items by type
N <- c("BABA WATOTO", "BATA", "BATA KANARI", "BATA MZINGA","BUNDI",  "CHAKIAMWEZI", 
       "CHAKIVALE",  "CHECHELE", "CHORE", "CHOZI", "FUFU", "FURUKOMBE", "KANGA", 
       "KASUKU", "KIBULUU", "KICHONGA", "KIHODI", "KIJIMBI MSITU", "KIJUNGU MALIKO", 
       "KINYONYOFUO", "KIPANGA", "KIPANGA ZUNGU", "KITWITWI", "KOBEA MINAZI", "KOHO",
       "KONGO", "KONGO HADIA", "KONGO MAJOKA", "KONGO MANGA", "KONGO NGOMBE", "KONGO WEUPE",
       "KOZI", "KUKU", "KUKU KUCHI", "KUKU PONI", "KUKU WA PWANI", "KUKU ZIWA", "KULASTARA",
       "KUNGURU", "KUNGURU WA BARA", "KUNGURU WA UNGUJA", "KUNGURU WA PEMBA", 
       "KWARARA", "KWAU", "KWECHE", "KWEMBE", "MAKAME KIJASI", "MANJA", "MBILIWILI",
       "MBILIWILI KIBATI", "MBUNI", "MEMBE", "MNANA", "MNANA CHANJAA", "MNANA WA KWECHE", 
       "MRAMBA", "MWEWE", "NINGA", "NJIWA", "NJIWA MANGA", "NJIWA MWITU", "NZI",
       "PANGA ZUNGU", "POPO MGOMBA", "POPO", "PUGI", "PUGI KIKOMBE", "PWERA", "SALILE",
       "SAUTI", "SAUTI KIPANGA", "SHAKE", "TAUSI", "TETERE", "TIGA", "TONGO", "VIPOPO", 
       "VUNJA JUNGU", "YOMBEYOMBE", "ZIWARDE",
       "CHANJE", "CHANJE MOTO", "CHANJE SUELE", "CHANJE UZIWA", "DAKTARI WA NGOMBE", 
       "CHONJWE", "MKRIMU", "MRAMBA BARA", "TATARA")
S <- c("BAKARI KICHWA", "BOCHO", "BOMBO", "BOMBO MTOJU", "BOMBO SHIMO",  
       "BULIBULI", "BUMBULA", "BUNJU", "BUNJU MIBA", "BUNJU MPIA", 
       "BUNJU TOTOVU", "CHAA", "CHANGU", "CHANGU KIDOA", "CHANGU NOOMO", "CHELEMA", 
       "CHEWA", "CHONGOE", "CHORE", "CHUI", "DAGAA", "DAGAA KIWEMBE", 
       "DAGAA MDUNJI", "DAGAA MSUMARI", "DAGAA SARADINI", "DUNGAZI", "DUVI", "JODARI", 
       "KAMAMBA", "KAMBA DOKO", "KAMBA UZI", "KAMBARE", "GOLDI", "GONGEO", "HAMSINI",
       "HUMSA", "KAMBISI", "KANDAZA", "KANDE", "KANGAJA", "KANGU", "KARARE", "KENENGWA",
       "KIBAMBA", "KIBOMA", "KIBORA", "KIBUA", "KICHALE", "KICHUNA",
       "KICHUNA NGOZI", "KIJUNGU MAFUTA", "KINYENGA", "KISUMBA", "KOANA", 
       "KOLEKOLE", "KOLEKOLE KIDIZARI", "KOLEKOLE SALEHE", "KOLEKOLE UZI", "KUBU", "KUNDAJI",
       "MBONO", "MBONO BLU", "MCHAKUFA", "MCHECHE", "MCHECHE BAPA", "MDUNJI",
       "MKALAMU", "MKUNGA", "MKUNGA CHUI", "MKUNGA MAREMBO", "MKUNGA MTO",
       "MKUNGA MWEUPE", "MKUNGA PAKA", "MORANI", "MPIA", "MSUSA",
       "MTURUTURU", "MWEWE", "MWIVA", "MZIPWE", "MZIRA", "NDODOZI", "NDUARO", 
       "NDUARO MBUYU", "NDUGUDI", "NGISI", "NGISI GOME", "NGISI MWANZI", "NGOGO",
       "NGOMBE WA PWANI", "NGAWA", "NGURU", "NGUVA", "NJANA", "NJANA NDOMO",
       "NUNGU", "NUNGU IVI", "NUNGU MIBA", "NYANGUMI", "NYENGA", "PAANZI", "PANJI",
       "PAPA", "PAPA KOJO", "PAPA UPANGA", "PARAGUNDA", "PEREGE", "POMBOO", "PONO",
       "PONO MAFUA", "PONO MWANI", "PUJU", "PUJU UPEMBE", "PWEZA", "PWEZA JOWEZI",
       "PWEZA MWANDAA", "SAMAKI MAJIBA", "SANGE", "SEHEWA", "SONGORO", "TAA", 
       "TANGARANGA", "TASI", "TAZO", "TEMBO", "TEMBO KIDOA", "TEMBO UZI", "TENGA",
       "TOTOVU", "TUMBO", "TUMBO KOTI", "TUNA", "UNA", "UNA MACHO", "UNGA KUKU", 
       "USUNGU", "VIROHO", "VUMBUKA", "WAYO",
       "ALI JUMA", "BOMBO RESI", "BUNJU CHUI", "KAMBA", "SADA KIREMBO") #"BOMBWE","BOMBWE RESI","DAGAA KIBAMBA","KIBOMBO", "KIBOMBWE", "KISANGE","MABULIBULI","MKUNGA WA PWANI", 
W <- c("BUKU", "CHATU", "CHECHE", "CHESI", "CHIMBACHI (MDUDU)", "CHUI", "DRAGONI", 
       "FARASI", "FISI", "KANGAROO", "KIBOKO", "KIFARU", "KIMA", "KIMA PUNJU", "KOMBA",
       "KONDOO", "KOROBWE", "MAJIBWA", "MAMBA", "MBWA", "MBUZI", "MBWA MWITU" , "NGAWA",
       "NGAMIA", "NGEDERE", "NGOMBE", "NGOMBE MWITU", "NGURUWE", "NYANI", "NYATI",
       "NYUMBU", "PAKA", "PANYA", "PELELE", "PAA", "POPO", "PUNDA", "PUNDAMILIA",
       "SIMBA", "SOKWE", "SUNGURA", "SWALA", "TEMBO", "TUMBILI", "TWIGA", "ZEBRA",
       "BEBERU", "GORILLA")
D <- c("BUIBUI", "BUUNZI", "CHAMVI", "CHUMA MBUZI", "CHUNGUCHUNGU", "CHURA", "DOKAA", 
       "DUDU MBUYU", "FUNGAPINGU", "FUNZA", "GANGAWIA", "JONGOO", "JONGOO CHECHE", 
       "JONGOO LA PWANI", "JONGOO MWITU", "JONGOO TAMBI", "JUSIJUSI", "JUSKAFIRI", 
       "KAA", "KADONDO", "KADONDO MAVI", "KAFKONDE", "GURUGURU", "HARIRI", "IENENDA NGUO", 
       "KANENE", "KARAKAKA", "KASA", "KAYAKAYA", "KENGE", "KICHOMVI", 
       "KIJINU", "KILUILUI", "KIMETEMETE", "KINUKAVUNDU", "KINYONGA", "KIPEPEO", "KIPIMA",
       "KIROBOTO", "KISAKUNDE", "KOBE", "KOKOKO", "KOME", "KONYESA", 
       "KORONGONJO", "KOYOKOYO", "KUNGUNI", "KUPA", "KUPE", "KURUMBIZA", "MAKAU", "MAKOME",
       "MAJOKA", "MATANDU", "MBURUNZI", "MBUUE", "MCHWA", "MENDE", "MJUSI", "MKUNGUGU",
       "MMBU", "MNYOO WA PWANI", "MSUELE", "MTI JUU", "MWARI", "MWATA", 
       "MZENENGO", "NKWI", "NNGE", "NYALE", "NYAMATA", "NYENJE", "NYOKA", "NYOKA WA PWANI",
       "NYUKI", "NYUNGWINYUNGWI", "NYUO", "PAANZI", "PANGA", "PANGA NOLE", "PARAPE",
       "PEPE", "POPO", "RENGA PAU", "SAGATOPE", "SASINDA", "SIAFU", "SISIMIZI", 
       "SISIMIZI JICHWA", "SISIMIZI MBIOMBIO", "SISIMIZI SIAFU", "TANDU", "UKEWE",
       "UKEWE WA PWANI", "UVI NYUNDO", "UVI", "UTITIRI", "USUBI", "ZINGADONDA",
       "CHAZA", "FUKULILE", "FUKULILE MTAMBWE",
       "CHEPU", "KOMBE", "TONDO", "UKUKWI") #"KICHAMVICHAMVI", "KOMBE LA PWANI", "MWATA LA PWANI",
M <- c("BEGU", "BUKOBA", "BUSTANI", "KABICHI", "KAJAKAJA", "HAUNGONGWA", "KIFA UONGO",
       "KIJIMBI", "KIRUKIA", "KITANGO", "KITUNGUU", "KITUNGUU SAUMU", "KIVUMBASI", 
       "KUNDE", "KUNDE NYKA", "MAHARAGUE", "MAHARI YA PAKA", "MATANGO", "MBILINGANI", 
       "MBIRIMBI", "MBIRIMBI WA KIZUNGU", "MBONO", "MBUNGO", "MBUNGO WA KIZUNGU",
       "MBUNI", "MBURA", "MBUYU", "MCHAICHAI", "MCHAPIA TUMBILI", "MCHEKECHU", "MCHEKWA",
       "MCHENZA", "MCHENZA MSITU", "MCHICHA", "MCHICHA BONDE", "MCHIKICHI", "MCHOCHA",
       "MCHOCHONI", "MCHONGOMA", "MCHOROKO", "MCHU", 'MCHUNGWA', "MCHUNGWA KALI",
       "MCHUNGWA KITAMU", "MCHUOA ULIMI", "MDALASINI", "MDAMUDAMU", "MCHEKUNDU", "MDIMU",
       "MDODO", "MDORIANI", "MDRESI", "MDUN GUDUNGU", "MFENESI", "MFIAGIO", "MFUU", 
       "MGOMBA", "MGOMBA MSITU", "MGULELE", "MHARITA", "MNANASI", "MIWA", 
       "MJIMBI", "MKADI", "MKANDAA", "MKANDAA MWEKUNDU", "MKANDAA MWEUSI", "MKANGAGA", 
       "MKANJA", "MKARAFUU", "MKARATI", "MKARATUSI", "MKARKADE", "MKEKEWA", "MKESHIA", 
       "MKINDU", "MKOKO", "MKOKWA", "MKOMAMANGA", "MKONO WATEMBO", "MKOROSHO",
       "MKUNAZI", "MKUNDE", "MKUNDE NYIKA", "MKUNGU", "MKUNGUMA", "MKUU WA USIKU",
       "MKUYU", "MKWAJU", "MKWAMBA", "MLANDEGE", "MLANGILANGI", "MLIMAU", "MLOZI", "MLUA",
       "MMUMUNYA", "MNAMIA MAJI", "MNAZI", "MNYAMBONYAMBO", "MNYANYA", "MPACHA", "MPAPAI", 
       "MPAPINDI", "MPARACHICHI", "MPATA KUVA", "MPEA", "MPEASI", "MPELEWA", "MPERA", 
       "MPESHENI", "MPILIPILI", "MPILIPILI HOHO", "MPILIPILI KICHAA", "MPILIPILI MANGA", 
       "MPILIPILI MBOGA", "MPILIPILI MBUZI", "MPILIPILI SIGARA", "MPINDAPINDAPO",
       "MPIRIKICHI", "MPO", "MPOFUA MACHO", "MPOPOO", "MPUNGA", "MPURAPUMBU", "MRIBA", 
       "MRIBA BONDE", "MRIHANI", "MSABUNI", "MSAJI", "MSANAKA", "MSASA", 
       "MSATARI", "MSEIPRAS", "MSHELISHELI", "MSHEMBELI", "MSHOKISHOKI", "MSHUBILI", 
       "MSIKUNDAZI", "MSINDUZI", "MSISIMIZI", "MSONOBARI", "MSOO", "MSTAFELI", "MSUFI",
       "MSUTI", "MTAKAWA", "MTAKAWA PWANI", "MTAMA", "MTANDAKANGA", "MTANGAWIZI", 
       "MTANGO", "MTENDE", "MTENGELE", "MTI ULAYA", "MTIKTI", "MTOMOKO", "MTOMONDO",
       "MTONDOO", "MTONGA", "MTUFAA", "MTUFAA LA KIZUNGU", "MTUGUU", "MTULE", "MTUMBAKU",
       "MTUNGUJA", "MTUNGULE", "MTUPA", "MTUTUTU", "MUA", "MUANZI", "MTUTUTU",
       "MUA", "MUANZI", "MUEMBE", "MUEMBE BAADI", "MUEMBE BAKORA", "MUEMBE BORBO", 
       "MUEMBE BWARE", "MUEMBE DODU", "MUEMBE KECHE", "MUEMBE KISUKARI", "MUEMBE KOMO",
       "MUEMBE MOSHI", 'MUEMBE NJURE', "MUEMBE NYONYO", "MUEMBE PAPAI", "MUEMBE PEMBENI", "MUEMBE PUNDA",
       "MUEMBE SAKUA", "MUEMBE SIAGI", "MUHALIZETI", "MUHINA", "MUHINA MWITU", 
       "MUHINDI", "MUHOGO", "MUHOGO PIRA", "MUILIKI", "MUINGA JINI", "MUIVUIVU", "MUWARDI",
       "MVANILLA", "MVINJE", "MVUJE", "MVULE", "MVUMANYUKI", "MVUMO", "MVUNJASHOKA",
       "MWALE", "MWAMBO", "MWANI", "MWANI MTIMBI", "MWAROBAINI", "MWASMINI", "MWAVI",
       "MYUNGIYUNGI", "MZABIBU", "MZAITUNI", "MZALIA NYUMA", "MZAMBARAU", "NDUMA", 
       "NGANO", "NYASI", "UNYONDWE", "UWANGA", "UYOGA", "UZILE", "KIAZI", "VIKUA", 
       "VIKUWA", "VIPO", "VITORI", "WENI",
       "KARANGA", "KAROTI", "KIAZI KIKUU", "KIAZI KITAMU", "MBELUNGI", 'MBAAZI', 
       "TUNGULE")#""MBOGA", "MIANZI","MSAKUA", "MTUNGUU", "MUHARITA",
#add others from list - which are NA but you know
# #DOUBLE <- C("CHORE", "CHUI", "MBONO", "MBUNI", "MWEWE", "NGAWA", "PAANZI", "POPO", 
#             "TEMBO")#POPO 3X
# #CHECK <- C("MKARKADE", "MDAMUDAMU", "MCHEKUNDU", "MPILIPILI SIGARA", "MUEMBE BAKORA", 
#            "MWEMBE DODU", 'MUEMBE NJURE', "MVANILLA", "NUNGU IVI", "SAUTI KIPANGA",
#            "TUNA", "CHAZA", "FUKULILE", "FUKULILE MTAMBWE",
#            "CHANJE", "CHANJE MOTO", "CHANJE SUELE", "CHANJE UZIWA", "DAKTARI WA NGOMBE")
###############
#add column
freelists$type <- ifelse( freelists$response  %in% N, "N", 
                  ifelse( freelists$response  %in% S, "S", 
                  ifelse( freelists$response  %in% W, "W", 
                  ifelse( freelists$response  %in% D, "D", 
                  ifelse( freelists$response  %in% M, "M", NA)))))

#check results
all_items <- freelists %>% group_by(response) %>% count()
all_items <- all_items[-1,]
all_items$type <- ifelse( all_items$response  %in% N, "N", 
                  ifelse( all_items$response  %in% S, "S", 
                  ifelse( all_items$response  %in% W, "W", 
                  ifelse( all_items$response  %in% D, "D", 
                  ifelse( all_items$response  %in% M, "M", NA)))))


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
correct_answers <- read.csv("2_Data_preparation/anonymized_data/questions_correct_answers.csv")
correct_answers <- correct_answers %>%  select(QN.number, Right.answer, area ) %>% 
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
image_answers <- read.csv("2_Data_preparation/anonymized_data/recognition_correct_answers.csv")

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

#organize type of questions
type_r <- unique(recognition$picture_id)
type_r[ str_detect(type_r, "bird", negate = FALSE)] <- "bird"
type_r[ str_detect(type_r, "plant", negate = FALSE)] <- "plant"
type_r[ str_detect(type_r, "tree", negate = FALSE)] <- "plant"
type_r[ str_detect(type_r, "palm", negate = FALSE)] <- "plant"
type_r[ str_detect(type_r, "root", negate = FALSE)] <- "plant"
type_r[ str_detect(type_r, "fruit", negate = FALSE)] <- "fruit"
type_r[ str_detect(type_r, "animal", negate = FALSE)] <- "animal"
type_r[ str_detect(type_r, "fish", negate = FALSE)] <- "sea"
type_r[ str_detect(type_r, "shell", negate = FALSE)] <- "sea"
type_r[ str_detect(type_r, "coral", negate = FALSE)] <- "sea"
type_r[ str_detect(type_r, "cucumber", negate = FALSE)] <- "sea"
type_r[ str_detect(type_r, "insect", negate = FALSE)] <- "insect"

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
d <- list( N = as.integer(nrow(interviews)),             #n individuals
           H = as.integer(length(unique(interviews$hh_id))),#n households
           HH= as.integer(interviews$hh_id),              #integer for household
           A = as.integer(interviews$summary_age) ,              #standardized age
           S = interviews$sex,                           #sex of individuals
           SY= as.integer(interviews$class_new),         #standardized n of years of school
           am= activities,                               #activities practiced
           C = ncol(activities),                         #n of activities
           type_l = all_items$type,                      #type of items freelist
           type_q = correct_answers$area,                #type of items questions
           type_r = type_r,                               #type of items picture recognition
           L = ncol(Y_l),                                #n of items in freelist
           Q = ncol(Y_q),                                #n of items in questions
           R = ncol(Y_r),                                #n of items in picture recognition
           Y_l = Y_l[order(rownames(Y_l)), ],            #matrix of answers for freelist
           Y_q = Y_q[order(rownames(Y_q)), ],            #matrix of answers for questions
           Y_r = Y_r[order(rownames(Y_r)), ])            #matrix of answers for picture recognition

mode(d$Y_l) <- "integer"
mode(d$Y_q) <- "integer"
mode(d$Y_r) <- "integer"
mode(d$am) <- "integer"


list.save(d, '2_Data_preparation/processed_data.RData')

rm( all_items, 
    activities, 
    ages,
    correct_answers, 
    chores,  
    edu_levels,
    extra_answers, 
    extra_qns, 
    freelists, 
    freelist_corrections,
    image_answers,
    interviews,
    na.activities,
    new_rows,
    not_a_creature,
    ql, qs,
    questionnaire,
    recognition,
    type_r,
    D, M, W, N, S,
    Y_l, Y_q, Y_r)


