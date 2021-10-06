##############################
######## MAKEFILE ############
##############################

#####################
#prepare environment#
#####################

#setwd to "Children_eco_knowledge/"

#load packages
library(rethinking)   #analyses, data and figures
library(tidyverse)    #data wrangling
library(rlist)        #loading list d
library(scales)       #
library(rlist)        #
library(ggfree)       #ridgeplots
library(gtools)       #


##############
#process data#
##############
#input: raw data in "2_Data_preparation/anonymized_data"
#output: list d with all data in "2_Data_preparation"
#time 2.79 min
start_time <- Sys.time()
source("2_Data_preparation/1_process_data.R")
end_time <- Sys.time()
time_1data <- end_time - start_time
time_1data

###############
#main analyses#
###############
#input: list d in "2_Data_preparation", stan models in "models"
#output: fit models in "3_Analysis/fit_models"
#time 21.02326 hours
start_time <- Sys.time()
source("3_Analysis/1_analysis.R")
end_time <- Sys.time()
time_2an <- end_time - start_time
time_2an

########################
#supplementary analyses#
########################
#input: list d in "2_Data_preparation", stan models in "models/supplementary_models"
#output: fit models in "3_Analysis/fit_models"
start_time <- Sys.time()
source("3_Analysis/3_supplementary_analyses.R")
end_time <- Sys.time()
time_3suppan <- end_time - start_time
time_3suppan
#################################
#prepare environment for figures#
#################################
#input: list d in "2_Data_preparation", fit models in "3_Analysis/fit_models"
#output: enviornment ready for making figures
start_time <- Sys.time()
source("4_Outputs/0_set_the_plot_stage.R")
end_time <- Sys.time()
time_4setstage <- end_time - start_time
time_4setstage
##############
#make figures#
##############
#input: list d in "2_Data_preparation", fit models in "3_Analysis/fit_models", through "4_Outputs/0_set_the_plot_stage.R"
#functions: in "4_Outputs/0_set_the_plot_stage.R"
#output: png files in 4_Outputs/plots"
start_time <- Sys.time()
source("4_Outputs/1_figures.R")
end_time <- Sys.time()
time_5fig <- end_time - start_time
time_5fig
############################
#make supplementary figures#
############################
#input: list d in "2_Data_preparation", fit models in "3_Analysis/fit_models", through "4_Outputs/0_set_the_plot_stage.R"
#functions: in "4_Outputs/0_set_the_plot_stage.R"
#output: png files in 4_Outputs/plots/supplementary_figures"
start_time <- Sys.time()
source("4_Outputs/3_supplementary_figures.R")
end_time <- Sys.time()
time_6suppfig <- end_time - start_time
time_6suppfig