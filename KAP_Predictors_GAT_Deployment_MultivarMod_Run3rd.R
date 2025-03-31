####KAP Predictors of GAT Deployment Multivariate Model####

####Before running this script, you need to run the "KAP_Data_Cleaning_Run1st"

#Load packages we will use
library(car)
library(broom)
library(kableExtra)

#Create combined data set
gat_dep_all<- cbind(GATYN_dem[-183,], GATYN_att[-183,], GATYN_know[-183,]
                    , GATYN_env_dist) #Need to remove row 183 to align distance with addresses because one respondent did not include their address

gat_dep_all <- gat_dep_all[, -c(12, 24)] #Remove extra GAT Y/N variables


############### ******* MAIN GAT DEPLOYMENT MODEL ******* ################
#This is the model that was th result of screening variables individually BEFORE taking out least significant variables. 

gat_dep1 <- glm(GATYN ~ AG + CH_YN + Income_Coll + ConcernScore + EffectFavScore + IMC_Score + IN_Coll + DK + LK + SK + KS + TimeOut + NumOut + NumOtherOut + Water_Dist + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

#Model with all interactions does not run, which is why you do not see it here.

summary(gat_dep1)


m1 <- Anova(gat_dep1, type = 3)
#AIC: 144.65

m1 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")

##Begin backwards stepwise regression by removing least sigificant variables and checking for loss of fit of greater than 2 AIC units
#Taking out first LEAST significant variable: KS

#Step A
gat_dep2 <- glm(GATYN ~ AG + CH_YN + Income_Coll + ConcernScore + EffectFavScore + IMC_Score + IN_Coll + DK + LK + SK + TimeOut + NumOut + NumOtherOut + Water_Dist + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep2)

#AIC: 142.65
m2 <- Anova(gat_dep2, type = 3)

m2 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")

#AIC fit IMPROVED, so KS stays out


#Taking out next LEAST significant variable: Water_Dist
#Step B
gat_dep3 <- glm(GATYN ~ AG + CH_YN + Income_Coll + ConcernScore + EffectFavScore + IMC_Score + IN_Coll + DK + LK + SK + TimeOut + NumOut + NumOtherOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep3)
#AIC: 140.73

m3 <- Anova(gat_dep3, type = 3)

m3 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")

#AIC fit improved

#Taking out next LEAST significant variable: IMC_Score
gat_dep4 <- glm(GATYN ~ AG + CH_YN + Income_Coll + ConcernScore + EffectFavScore + IN_Coll + DK+ LK + SK + TimeOut + NumOut + NumOtherOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep4)
#AIC: 138.81

m4 <- Anova(gat_dep4, type = 3)

m4 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")

#Taking out next LEAST significant variable: DK
#Step D
gat_dep5 <- glm(GATYN ~ AG + CH_YN + Income_Coll + ConcernScore + EffectFavScore + IN_Coll + LK + SK + TimeOut + NumOut + NumOtherOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep5)

#AIC:137.06
m5 <- Anova(gat_dep5, type = 3)

m5 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")
#AIC fit improved

#Taking out next LEAST significant variable: Age
gat_dep6 <- glm(GATYN ~ CH_YN + Income_Coll + ConcernScore + EffectFavScore + IN_Coll + LK + SK + TimeOut + NumOut + NumOtherOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep6)
#AIC: 134.36

m6 <- Anova(gat_dep6, type = 3)

m6 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")


#Remove LEAST significant variable: CH_YN (children yes/no)
gat_dep7 <- glm(GATYN ~ Income_Coll + ConcernScore + EffectFavScore + IN_Coll + LK + SK + TimeOut + NumOut + NumOtherOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep7)

#AIC 132.79

m7 <- Anova(gat_dep7, type = 3)

m7 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")


#Remove next LEAST significant variable: NumOtherOut (number of outdoor activities)
gat_dep8 <- glm(GATYN ~ Income_Coll + ConcernScore + EffectFavScore + IN_Coll + LK + SK + TimeOut + NumOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep8)

#131.62

m8 <- Anova(gat_dep8, type = 3)

m8 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")

#AIC fit improved

#Remove next LEAST significant variable: NumOut
gat_dep9 <- glm(GATYN ~ Income_Coll + ConcernScore + EffectFavScore + IN_Coll + LK + SK + TimeOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep9)

#AIC 130.38

m9 <- Anova(gat_dep9, type = 3)

m9 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")



#Remove next LEAST significant variable: LK
gat_dep10 <- glm(GATYN ~ Income_Coll + ConcernScore + EffectFavScore + IN_Coll + SK + TimeOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep10)

#AIC 130.54

m10 <- Anova(gat_dep10, type = 3)

m10 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")

#Remove next LEAST significant variable: IN_Coll
#Step D
gat_dep11 <- glm(GATYN ~ Income_Coll + ConcernScore + EffectFavScore + SK + TimeOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep11)

#AIC 131.31, increase, but by less than 2 units

m11 <- Anova(gat_dep11, type = 3)

m11 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")


#AIC fit improved

#####FINAL MODEL######
#Remove next LEAST significant variable: Concern Score
gat_dep12 <- glm(GATYN ~ Income_Coll + EffectFavScore + SK + TimeOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

summary(gat_dep12)
coef(gat_dep12)

#AIC 132.73 = loss of fit but not significant
m12 <- Anova(gat_dep12, type = 3)

m12 %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")

#Extract odds ratio from final model
exp(cbind(coef(gat_dep12), confint(gat_dep12)))

##Once you have run this script, plotting scrips can be run. Order of run for plotting scripts should not matter. 
