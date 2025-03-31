######Demographic and Environmental Predictors of Significant Knowledge and Attitude Predictors of GAT Deployment#####

###Run KAP_Data_Cleaning_Run1st before running this script

library(tidyverse)

#Join demographic and environmental variables with the knowledge and attitude variables that were significant in the final multivariate model 

demo_enviro_preds <- cbind(Demos_sub_c[-183,], env_sub_dist) #Need to remove one row from demographics because it otherwise won't align with distance measurements since one respondent entered his name instead of his address

att_know_var <- select(.data = Att_sub_c, EffectFavScore)

att_know_var <- cbind(att_know_var, Know_sub_c$SK)
att_know_var <- att_know_var[-183,]

mod_dat <- cbind(att_know_var, demo_enviro_preds)
mod_dat <- rename(.data = mod_dat, "SK" = "Know_sub_c$SK")

#Relevel Income
mod_dat$Income_R <- relevel(mod_dat$Income_Coll, ref=3)
  
###Run the GLM_Data_Prep_Run1st script before this script
library(car)
library(broom)
library(kableExtra)

#####Univariate Screening Step#####

#Demographic variables on GATP attitude (EffectFavScore). Input each demographic variable in order and then run following code to check for significance. 
glm1 <- glm(EffectFavScore ~ Edu_Coll, family = gaussian, data = mod_dat) #Run individually
coef(glm1)
confint(glm1)
summary(glm1)

m1<- Anova(glm1, type = 3)

m1 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )

#Environmental Variables on GATP attitude
#Input each demographic variable in order and then run following code to check for significance. 
glm2 <- glm(EffectFavScore ~ NumOtherOut, family = gaussian, data = mod_dat) #Run individually
coef(glm2)
confint(glm2)
summary(glm2)

m2 <- Anova(glm2, type = 3)

m2 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )

#Demographic Variables on Species Knowledge
#Input each demographic variable in order and then run following code to check for significance. 
glm3 <- glm(SK ~ Edu_Coll, family = binomial, data = mod_dat) #Run individually

exp(cbind(coef(glm3), confint(glm3)))

summary(glm3)

m3 <- Anova(glm3, type = 3)

m3 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )

#Environmental Variables on Species Knowledge
#Input each demographic variable in order and then run following code to check for significance. 
glm4 <- glm(SK ~ IN_Coll, family = binomial, data = mod_dat) #Run individually

exp(cbind(coef(glm4), confint(glm4)))

summary(glm4)

m4 <- Anova(glm4, type = 3)

m4 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )

####Multivariate Model#####
##GATP Attitude
####Demographic and Environmental Predictors of Overall Favorability Score: Multivariate
glmA <- glm(EffectFavScore ~ AG + GN + NH + CH_YN + YUP + NumOut + NumOtherOut + AG * GN + AG * CH_YN + AG * YUP + AG * NumOut + AG * NumOtherOut + GN * NH + GN * CH_YN + GN * YUP + GN * NumOut + GN * NumOtherOut + NH * CH_YN + NH * YUP + NH * NumOut + NH * NumOtherOut + YUP * NumOut + YUP * NumOtherOut + NumOut * NumOtherOut, family = gaussian, data = mod_dat) 

summary(glmA)

#AIC 689.62

#Remove all interactions and check for loss of fit
glmA <- glm(EffectFavScore ~ AG + GN + NH + CH_YN + YUP + NumOut + NumOtherOut, family = gaussian, data = mod_dat) 

summary(glmA)

#AIC 669.53, no loss of fit

#Now run ANOVA
m1 <- Anova(glm1, type = 3)

m1 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )

#Remove NumOtherOut
glm1 <- glm(EffectFavScore ~ AG + GN + NH + CH_YN + YUP + NumOut, family = gaussian, data = mod_dat) 

summary(glm1)

#AIC 667.55

m1 <- Anova(glm1, type = 3)

m1 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )


#Remove AG
glm1 <- glm(EffectFavScore ~ GN + NH + CH_YN + YUP + NumOut, family = gaussian, data = mod_dat) 

summary(glm1)

#AIC 662.96

m1 <- Anova(glm1, type = 3)

m1 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )

#Remove NH
glm1 <- glm(EffectFavScore ~ GN + CH_YN + YUP + NumOut, family = gaussian, data = mod_dat) 

summary(glm1)

#AIC 661.61

m1 <- Anova(glm1, type = 3)

m1 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )

#FIANL MODEL
#Remove GN
gatp_att_f<- glm(EffectFavScore ~ CH_YN + YUP + NumOut, family = gaussian, data = mod_dat) 
coef(gatp_att_f)
summary(gatp_att_f)

#Extract odds ratios
exp(cbind(coef(gatp_att_f), confint(gatp_att_f)))

#AIC 661.92 - minor loss of fit, but still within parameters

m1 <- Anova(gatp_att_f, type = 3)

m1 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )


#Species Knowledge
glm2 <- glm(SK ~ AG + NH + CH_YN + YUP + RO + TimeOut + NumOut + GreenSp_Dist, AG * NH + AG * CH_YN + AG * YUP + AG * RO + AG * TimeOut + AG * NumOut + NH * CH_YN + NH * YUP + NH * RO + NH * TimeOut + NH * NumOut + NH * GreenSp_Dist + CH_YN * YUP + CH_YN * RO + CH_YN * TimeOut + CH_YN * NumOut + CH_YN * GreenSp_Dist + YUP * RO + YUP * TimeOut + YUP * NumOut + YUP * GreenSp_Dist + RO * TimeOut + RO * NumOut + RO * GreenSp_Dist + TimeOut * NumOut + TimeOut * GreenSp_Dist + NumOut * GreenSp_Dist, family = binomial, data = mod_dat)

summary(glm2)
#Does not run with all interactions

#Remove all interactions
glm2 <- glm(SK ~ AG + NH + CH_YN + YUP + RO + TimeOut + NumOut + GreenSp_Dist, family = binomial, data = mod_dat)

summary(glm2)

#AIC 196.69

#Proceed to running ANOVA
m2 <- Anova(glm2, type = 3)

m2 %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )

#Remove NumOut -- FINAL MODEL

SK_f <- glm(SK ~ AG + NH + CH_YN + YUP + RO + TimeOut + GreenSp_Dist, family = binomial, data = mod_dat)
coef(SK_f)
summary(SK_f)

exp(cbind(coef(SK_f), confint(SK_f)))
#AIC: 194.7


#Proceed to running ANOVA
mf <- Anova(SK_f, type = 3)

mf %>% 
  tidy() %>% 
  kable(caption = "Demographic Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "rst"
  )

#Remove YUP

glmsk2 <- glm(SK ~ AG + NH + CH_YN + RO + TimeOut + GreenSp_Dist, family = binomial, data = mod_dat)

summary(glmsk2)

#AIC: 219.94, major loss of fit

