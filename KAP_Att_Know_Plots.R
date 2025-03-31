###Attitude and Knowledge Plots
library(tidyr)
library(emmeans)
###Run Enviro_Demo_Predictors of Attitudes and Knowledge script before running this one

#For GATP Attitude: Children (Yes/No), Years in University Park, Yard Activities
gatp_att_f<- glm(EffectFavScore ~ CH_YN + YUP + NumOut, family = gaussian, data = mod_dat)

#Extract odds ratios
exp(cbind(coef(gatp_att_f), confint(gatp_att_f)))

#Uses ggpredict to add the likelihood of GAT deployment based on the model to that dataframe
gatp_att <- emmeans(gatp_att_f, "CH_YN", type = "response")
gatp_att <- as.data.frame(gatp_att)


#Set text and title size and pic dimensions for consistency
txt_size <- 24
title_size <- 26
w <- 30
h <- 28
res <- 600

#Reset margins
par(mar=c(0,0,0,0))

#Plot GATP_att over children
GATP_att_CHYN <- ggplot(gatp_att, aes(x=CH_YN, y=emmean)) + 
  geom_col() +
  theme_classic() +
  geom_errorbar(aes(ymin=emmean - SE, ymax=emmean + SE), 
                width=0.05,colour="black", alpha=0.9, size=.5) +
  labs(x = "Children in Household", 
       y = "GATP Attitude Score") +
  scale_y_continuous(limits = c(0, 10), breaks = c(2,4,6,8,10)) +
  theme(axis.text = element_text(size = txt_size), axis.title = element_text(size = title_size))

GATP_att_CHYN

ggsave(filename = "GATP_att_CHYN.png", plot = GATP_att_CHYN, width = w, height = h, dpi = res, units = "cm", device = "png")



#Create data frame with significant predictors of GATP Att
GATP_preds <- select(.data = mod_dat, EffectFavScore, CH_YN, YUP, NumOut)
GATP_preds$score <- predict(gatp_att_f, newdata = GATP_preds, type = "response")

#Plot GATP Att and YUP
GATP_att_YUP <- ggplot(GATP_preds, aes(x = YUP, y = score)) +
  geom_smooth(colour = "black", method = glm, size = 1)+
  scale_x_continuous(limits = c(5, 52), breaks = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55))+
  xlab("Years in UP")+
  ylab("GATP Attitude Score")+
  scale_y_continuous(limits = c(2, 10), breaks = c(2,4,6,8,10))+
  theme_classic()+
  theme(axis.text = element_text(size = txt_size), axis.title = element_text(size = title_size))

GATP_att_YUP

ggsave(filename = "GATP_att_YUP.png", plot = GATP_att_YUP, width = w, height = h, dpi = res, units = "cm", device = "png")


#Plot GATP Att and Yard Activities
GATP_att_YA <- ggplot(GATP_preds, aes(x = NumOut, y = score)) +
  geom_smooth(colour = "black", method = glm, size = 1)+
  xlab("Number of Yard Activities")+
  ylab("GATP Attitude Score")+
  scale_y_continuous(limits = c(2, 10), breaks = c(2,4,6,8,10))+
  theme_classic()+
  theme(axis.text = element_text(size = txt_size), axis.title = element_text(size = title_size))

GATP_att_YA 

ggsave(filename = "GATP_att_YA.png", plot = GATP_att_YA, width = w, height = h, dpi = res, units = "cm", device = "png")


#For Species Knowledge: Home Ownership
SK_f <- glm(SK ~ AG + NH + CH_YN + YUP + RO + TimeOut + GreenSp_Dist, family = binomial, data = mod_dat)
sk <- emmeans(SK_f, "RO", type = "response")
sk <- as.data.frame(sk)
sk[2,3] <- 0.1 #Round SE value to avoid error bar going below zero

sk_ro <- ggplot(sk, aes(x=RO, y=prob)) + 
  geom_col() +
  theme_classic() +
  geom_errorbar(aes(ymin=prob - SE, ymax=prob + SE), 
                width=0.05,colour="black", alpha=0.9, size=.5) +
  labs(x = "Home Ownership Status", 
       y = "Odds of Correct Species Knowledge") +
  theme(axis.text = element_text(size = txt_size), axis.title = element_text(size = title_size))

sk_ro

ggsave(filename = "sk_ro.png", plot = sk_ro, width = w, height = h, dpi = res, units = "cm", device = "png")


#Generate multi-pane figure
library(gridExtra)

Att_know_plot <- grid.arrange(sk_ro, GATP_att_CHYN, GATP_att_YA, GATP_att_YUP, nrow = 2)

#Set text and title size and pic dimensions for consistency
txt_size <- 28
title_size <- 30

ggsave(filename = "Att_know_plot.png", plot = Att_know_plot, width = 60, height = 65, dpi = res, units = "cm", device = "png")

