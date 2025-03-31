####Categorical KAP Predictors of GAT Deployment Plots###

####Before running this, we need to run the data cleaning script and then the data frame creation and model lines below##

#Load pacakges
library(emmeans)
library(car)
library(ggplot2)

#Create combined dataset
gat_dep_all<- cbind(GATYN_dem[-183,], GATYN_att[-183,], GATYN_know[-183,]
                    , GATYN_env_dist)

gat_dep_all <- gat_dep_all[, -c(12, 20, 25)] #Remove extra GAT Y/N variables



#Run final multivariate model of GAT deployment
final_GAT_mod <- glm(GATYN ~ Income_Coll + EffectFavScore + SK + TimeOut + GreenSp_Dist, data = gat_dep_all, family = binomial(link = logit))


#Extract odds ratio from final model
exp(cbind(coef(final_GAT_mod), confint(final_GAT_mod)))
#View reference grid
ref_grid(final_GAT_mod)

coef(final_GAT_mod)
summary(final_GAT_mod)
Anova(final_GAT_mod)
#List of categorical predictors of GAT Deployment
#1)Income
#2) Species knowledge

#View model
summary(final_GAT_mod)


###Income
#Extract marginal means for income and save as data frame
inc <- emmeans(final_GAT_mod, "Income_Coll", type = "response")
inc <- as.data.frame(inc)

#Coerce GATYN to be numeric
gat_dep_all$GATYN <- as.numeric(as.character(gat_dep_all$GATYN))

#Order factors
inc$Income_Coll <- factor(inc$Income_Coll, levels = c("Up to $100,000", "$100,000 - $200,000", "$200,000 or greater"))
inc$Income_Coll <- factor(inc$Income_Coll, levels = c("Up to $100,000", "$100,000 - $200,000", "$200,000 or greater"), labels = c("Low", "Middle", "High"))

#Set text and title size and pic dimensions for consistency
txt_size <- 18
title_size <- 20
w <- 30
h <- 28
res <- 600

#Income plot
Inc_GATDep <- ggplot(inc, aes(x=Income_Coll, y=prob)) + 
  geom_col() +
  theme_classic() +
  geom_errorbar(aes(ymin=prob - SE, ymax=prob + SE), 
                width=0.05,colour="black", alpha=0.9, size=.5) +
  labs(x = "Household Income", 
       y = "Odds of GAT Deployment") +
  ylim(0,0.8) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = title_size))

Inc_GATDep

ggsave(filename = "IncGATDep.png", plot = Inc_GATDep, width = w, height = h, dpi = res, units = "cm", device = "png")


#Species knowledge
sk <- emmeans(final_GAT_mod, "SK", type = "response")
sk <- as.data.frame(sk)

sk$SK <- factor(sk$SK, levels = c(0,1), labels = c("No Knowledge", "Correct Knowledge"))


SK_GATDep <- ggplot(sk, aes(x=SK, y=prob)) + 
  geom_col() +
  theme_classic() +
  geom_errorbar(aes(ymin=prob - SE, ymax=prob + SE), 
                width=0.05,colour="black", alpha=0.9, size=.5) +
  labs(x = "Mosquito Species Knowledge", 
       y = "Odds of GAT Deployment") +
  ylim(0,0.8) +
  theme(axis.text = element_text(size = txt_size), axis.title = element_text(size = title_size))

SK_GATDep

ggsave(filename = "SK_GATDep.png", plot = SK_GATDep, width = w, height = h, dpi = res, units = "cm", device = "png")
