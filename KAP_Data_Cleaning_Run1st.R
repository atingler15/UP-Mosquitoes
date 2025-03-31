######KAP Questionnaire Generalized Linear Model Data Prep Script####

#Modify accordingly to your working directory before running code
setwd(".")

##This script prepares data to run subsequent model script. This script only cleans and organizes the data, it does not run any models.

#Detach all packages for clean run. Don't worry if this errors, it just means there are no pacakges to detach.
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

#First, let's read in the data, clean it, and separate into individual data frames as needed

#Read in the data. Be sure to modify the file location to where you have saves they R project. 
UP_GAT_Survey <- 
  read.csv("UP_GAT_Survey_Clean_MAIN.csv")

#Load packages
library(tidyverse)

#Recode missing values to consistent NA value. Here, I used base R to do this, and I know I have two types of NAs from the original excel data, "#N/A and an empty space represented by ""
UP_GAT_Survey[UP_GAT_Survey[, 1:108] == "#N/A"] <- NA
UP_GAT_Survey[UP_GAT_Survey[, 1:108] == ""] <- NA
UP_GAT_Survey[UP_GAT_Survey[, 1:108] == "#VALUE!"] <- NA
UP_GAT_Survey[UP_GAT_Survey[, 1:108] == "Prefer to self-identify"] <- NA


#We are going to separate variables by type and code each subset of data, just to make thinks a little cleaer and easier to follow instead of doing the entire data frame all at once. 

#Pull all our demographic variabels into their own dataframe
Demos_sub <- select(.data = UP_GAT_Survey, AG, GN, NH, 
                    CH, CH_YN, PetOut, YUP, RO, 
                    Income_Coll, Edu_Coll)

#Let's recode variables to categorical factors and numerical continuous variables, as appropriate and save the cleaned version as Demos_sub_c

Demos_sub_c <-
  Demos_sub %>% 
  mutate(across(c(AG, GN), factor),
         CH_YN = as.factor(CH_YN),
         NH = as.numeric(NH),
         PetOut = as.numeric(PetOut),
         YUP = as.numeric(YUP),
         across(c(RO:Edu_Coll), factor))


##Now, we are going to add distance to this from another sheet

#First, let's read in the data
Dist_vars <- 
  read.csv("human_subj_ques_dists.csv")

##Let's clean this up a bit
Dist_vars_c <- Dist_vars[,c(4,5)] #subset just distance measurements
Dist_vars_c <- rename(.data = Dist_vars_c, #Rename column
                       "Water_Dist" = "Distance.to.Wells.Run..m.", 
                       "GreenSp_Dist" = "Distance.to.Town.Park")


#Pull knowledge variables
Know_sub <- select(.data = UP_GAT_Survey, DK, LK, SK, KS)

Know_sub_c <-
  Know_sub %>% 
  mutate(across(c(DK, LK, SK), factor))

Know_sub_c$DK <- factor(Know_sub_c$DK, levels = c(0,1), labels = c("No Knowledge", "Correct Knowledge"))
Know_sub_c$LK <- factor(Know_sub_c$LK, levels = c(0,1), labels = c("No Knowledge", "Correct Knowledge"))
Know_sub_c$SK <- factor(Know_sub_c$SK, levels = c(0,1), labels = c("No Knowledge", "Correct Knowledge"))

UP_GAT_Survey[UP_GAT_Survey[, 1:108] == "Prefer to self-identify"] <- NA
#Pull attitude variables
Att_sub <- select(.data = UP_GAT_Survey, MM,
                  ConcernScore, EffectFavScore,
                  IMC_Score, UPF, KN)

#Clean Attitudes subset
Att_sub_c <-
  Att_sub %>% 
  mutate(across(c(UPF, KN), factor),
         MM = as.factor(MM))

#Now, let's pull the practice and environmental variables from the dataset
#We will pull them all for future analyses, but for now we  will focus on GAT deployment
PracEnv_sub <- select(.data = UP_GAT_Survey, GAT21, GAT2021, GATPr, 
                   GATPrev, NumSummers, CHM, CHMN, EC, EmptyConts, 
                   UD, Dunks, TOD, IN_Coll, TimeOut, NumOut, NumOtherOut)

PracEnv_sub_c <-
  PracEnv_sub %>% 
  mutate(across(c(GAT21:GATPrev), factor),
         across(c(CHM:IN_Coll), factor),
         NumSummers = as.numeric(NumSummers),
         TimeOut = as.numeric(TimeOut),
         NumOut = as.numeric(NumOut),
         NumOtherOut = as.numeric(NumOtherOut))

#GAT Deployment and Environmental Relationships
env_sub <- PracEnv_sub_c[, c(13:16)] #Pull enviro relations variables from practice subset
env_sub_dist <- env_sub[-183, ] #Remove last row to join with distance to water measures
env_sub_dist <- cbind(env_sub_dist, Dist_vars_c) #Join with distance to water measures


#Now, let's bind the GAT deployment variable to 
#our subsets for demographics, attitudes, and knowledge

#This will make running the screening tests simpler

#GAT Deployment and Demographics
GATYN_dem <- cbind(PracEnv_sub_c$GAT2021, Demos_sub_c)
GATYN_dem <- rename(.data = GATYN_dem, "GATYN" = "PracEnv_sub_c$GAT2021")

#GAT Deployment and Attitudes
GATYN_att <- cbind(PracEnv_sub_c$GAT2021, Att_sub_c)
GATYN_att <- rename(.data = GATYN_att, "GATYN" = "PracEnv_sub_c$GAT2021")

#GAT Deployment and Knowledge
GATYN_know <- cbind(PracEnv_sub_c$GAT2021, Know_sub_c)
GATYN_know <- rename(.data = GATYN_know, "GATYN" = "PracEnv_sub_c$GAT2021")

#GAT Deployment and Environmental Relationships
GATYN_env <- cbind(PracEnv_sub_c$GAT2021, env_sub) #Join GAT dep var with env vars
GATYN_env <- rename(.data = GATYN_env, "GATYN" = "PracEnv_sub_c$GAT2021") #rename GAT dep


prac_dist <- PracEnv_sub_c[-183,] #Remove last row to join with distance to water measures

GATYN_env_dist <- cbind(prac_dist$GAT2021, env_sub_dist) #Join with distance to water measures

GATYN_env_dist <- rename(.data = GATYN_env_dist, "GATYN" = "prac_dist$GAT2021") #rename GAT dep


