####KAP Univariate Screening####

#Load packages for regression 
library(car)
library(broom)
library(kableExtra)


#Create dataframe with all demographic variables
GATYN_dem$Income_R <- relevel(GATYN_dem$Income_Coll, ref=3)

####Demographic Variable Screening####
#Run demographic variables individually by changing predictor variable in the code line below
glm1 <- glm(GATYN ~ Edu_Coll, family = binomial, data = GATYN_dem) 

exp(cbind(coef(glm1), confint(glm1)))

#Run Type III ANOVA on glm object
m1 <- Anova(glm1, type = 3)

#View summary table of ANOVA results
m1 %>% 
  tidy() %>% 
  kable(caption = "Attitude Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 10),
        format = "rst"
  )


####Environmental Variable Screening####
#Run environmental variables individually by changing predictor variable in the code line below
glm2 <- glm(GATYN ~ NumOtherOut, family = binomial, data = GATYN_env_dist) 

exp(cbind(coef(glm2), confint(glm2)))

coef(glm2)

#Run Type III ANOVA on glm object
m2 <- Anova(glm2, type = 3)

#View summary table of ANOVA results
m2 %>% 
  tidy() %>% 
  kable(caption = "Attitude Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 10),
        format = "rst"
  )


####Attitude Variable Screening####
#Note: UPF (friendliness of UP), and KN (know neighbors), and MM (Most Mosquitoes) do not have large enough groups to run in multivariate model

#Run attitude variables individually by changing predictor variable in the code line below
glm3 <- glm(GATYN ~ IMC_Score, family = binomial, data = GATYN_att) 

exp(cbind(coef(glm3), confint(glm3)))

coef(glm3)

#Run Type III ANOVA on glm object
m3 <- Anova(glm3, type = 3)

#View summary table of ANOVA results
m3 %>% 
  tidy() %>% 
  kable(caption = "Attitude Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 10),
        format = "rst"
  )

####Knowledge Variable Screening####
#Run knowledge variables individually by changing predictor variable in the code line below
glm4 <- glm(GATYN ~ SK, family = binomial, data = GATYN_know) 

exp(cbind(coef(glm4), confint(glm4)))


coef(glm4)

#Run Type III ANOVA on glm object
m4 <- Anova(glm4, type = 3)

#View summary table of ANOVA results
m4 %>% 
  tidy() %>% 
  kable(caption = "Attitude Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 10),
        format = "rst"
  )
