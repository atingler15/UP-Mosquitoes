#######Plotting Model Trend Lines for Continuous KAP Predictors of GAT Deployment######

#List of continuous predictors predicting GAT deployment
  #1) GATP Attitude
  #2) Time Outdoors per week
  #3) Distance to green space

library(tidyverse)
library(ggplot2)
library(car)
library(broom)
library(kableExtra)

####Before running this, we need to run the data cleaning script and then the data frame creation and model lines below##

#Create combined dataset
gat_dep_all<- cbind(GATYN_dem[-183,], GATYN_att[-183,], GATYN_know[-183,]
                    , GATYN_env_dist)

gat_dep_all <- gat_dep_all[, -c(12, 20, 25)] #Remove extra GAT Y/N variables


#Run final multivariate model of GAT deployment to extract model trendlines later
final_GAT_mod <- glm(GATYN ~ Income_Coll + EffectFavScore + SK + TimeOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))

#View the model if desired
summary(final_GAT_mod)
mf <- Anova(final_GAT_mod, type = 3)

mf %>% 
  tidy() %>% 
  kable(caption = "Multivariate Predictors of GAT Deployment in 2021",
        col.names = c("Predictor", "Chi Sq", "Df", "p"),
        digits = c(0, 5, 2, 5),
        format = "simple")

#Create data frame with significant predictors of GAT deployment
GAT_preds <- select(.data = gat_dep_all, GATYN, Income_Coll, EffectFavScore, SK, TimeOut, GreenSp_Dist)

#Uses ggpredict to add the likelihood of GAT deployment based on the model to that dataframe
GAT_preds$prob_gat <- predict(final_GAT_mod, newdata = GAT_preds, type = "response")

#Set text and title size and pic dimensions for consistency
txt_size <- 18
title_size <- 20
w <- 30
h <- 28
res <- 600

###Begin plotting using ggplot2
#GATP Attitude
GAT_GATPAttitude<- ggplot(GAT_preds, aes(x = EffectFavScore, y = prob_gat)) +
  geom_smooth(colour = "black", method = glm, size = 1)+
  xlab("GATP Attitude")+
  ylab("Odds of GAT Deployment")+
  theme_classic()+
  xlim(2,10) +
  theme(axis.text = element_text(size = txt_size), axis.title = element_text(size = title_size))

GAT_GATPAttitude

ggsave(filename = "GAT_GATPAttitude.png", plot = GAT_GATPAttitude, width = w, height = h, dpi = res, units = "cm", device = "png")

#Adjust plot margins
par(mar=c(0,0,0,4))

#Time Outdoors per week
GAT_TimeOut<- ggplot(GAT_preds, aes(x = TimeOut, y = prob_gat)) +
  geom_smooth(colour = "black", method = glm, size = 1)+
  scale_y_continuous(limits = c(0, 1))+
  scale_x_continuous(limits = c(0, 9), breaks = c(0:9))+
  xlab("Time Outdoors (hrs/wk)")+
  ylab("Odds of GAT Deployment")+
  ylim(0,0.8)+
  theme_classic()+
  theme(axis.text = element_text(size = txt_size), axis.title = element_text(size = title_size))

GAT_TimeOut

ggsave(filename = "GAT_TimeOut.png", plot = GAT_TimeOut, width = w, height = h, dpi = res, units = "cm", device = "png")

#Greenspace plot
GAT_GreenSP <- ggplot(GAT_preds, aes(x = GreenSp_Dist, y = prob_gat)) +
  geom_smooth(colour = "black", method = glm, size = 1)+
  scale_y_continuous(limits = c(0, 0.75))+
  scale_x_continuous(limits = c(0, 700), breaks = c(100, 200, 300, 400, 500, 600, 700))+
  xlab("Distance to Town Greenspace (m)")+
  ylab("Odds of GAT Deployment")+
  ylim(0,0.8)+
  theme_classic()+
  theme(axis.text = element_text(size = txt_size), axis.title = element_text(size = title_size))


GAT_GreenSP

ggsave(filename = "GAT_GreenSp.png", plot = GAT_GreenSP, width = w, height = h, dpi = res, units = "cm", device = "png")

###Multipanel plot
#Beore running the multi_plot, you need to generate plots for the categorical and continuous predictors
library(gridExtra)

multi_plot <- grid.arrange(Inc_GATDep, GAT_GreenSP, GAT_TimeOut, SK_GATDep, 
                           GAT_GATPAttitude, nrow = 2)

ggsave(filename = "multi_plot.png", plot = multi_plot, width = 45, height = 30, dpi = res, units = "cm", device = "png")


#Extract odds ratios
exp(cbind(coef(final_GAT_mod), confint(final_GAT_mod)))
