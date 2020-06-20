# install packages
install.packages('nnet', repos = "http://cran.us.r-project.org")
install.packages('tidyverse', repos = "http://cran.us.r-project.org")
install.packages('dplyr', repos = "http://cran.us.r-project.org")
install.packages('arm', repos = "http://cran.us.r-project.org")
install.packages('plyr', repos = "http://cran.us.r-project.org")
install.packages('randomForest', repos = "http://cran.us.r-project.org")
install.packages('caTools', repos = "http://cran.us.r-project.org")
install.packages('partykit', repos = "http://cran.us.r-project.org")

# load packages
library(tidyverse)
library(dplyr)
library(nnet) # for multinom
library(plyr) # for count
library(knitr) # for kable
library(randomForest) # for random forest
library(caTools) # for random forest
library(partykit) # for ctree

#load data
load("data/df.RData")
load("data/df_train.RData")
load("data/df_test.RData")

mcr=NULL

mcr=NULL

# function to compute missclassification rate
avclassifyrate=function(data, model){
  rating=l1=l2=l3=l4=l0=NULL
  for (i in 1:5){
    tf=c(sample(as.numeric(rownames(subset(df, label==0))),nrow(subset(df, label==0))*0.8), sample(as.numeric(rownames(subset(df, label==1))),nrow(subset(df, label==1))*0.8),sample(as.numeric(rownames(subset(df, label==2))),nrow(subset(df, label==2))*0.8), sample(as.numeric(rownames(subset(df, label==3))),nrow(subset(df, label==3))*0.8), sample(as.numeric(rownames(subset(df, label==4))),nrow(subset(df, label==4))*0.8))
    t_rain <- data[tf,]
    t_est <- data[-tf,]
    m1<- model
    # column of predicted classes
    t_est$predictions <- predict(m1, t_est)
    # 0-1 loss (overall)
    loss <- ifelse(t_est$label != t_est$predictions, 1, 0)
    rating <- c(rating, sum(loss==1)/length(loss)) #overall misclassification rate
    #missclassified0
    sub0<- subset(t_est, label==0)
    loss0 <- ifelse(sub0$label != sub0$predictions, 1, 0)
    l0<- c(l0, sum(loss0==1)/length(loss0))
    #missclassified1
    sub1<- subset(t_est, label==1)
    loss0 <- ifelse(sub1$label != sub1$predictions, 1, 0)
    l1<- c(l1, sum(loss0==1)/length(loss0))
    #missclassified2
    sub2<- subset(t_est, label==2)
    loss0 <- ifelse(sub2$label != sub2$predictions, 1, 0)
    l2<- c(l2, sum(loss0==1)/length(loss0))
    #missclassified3
    sub3<- subset(t_est, label==3)
    loss0 <- ifelse(sub3$label != sub3$predictions, 1, 0)
    l3<- c(l3, sum(loss0==1)/length(loss0))
    #missclassified4
    sub4<- subset(t_est, label==4)
    loss0 <- ifelse(sub4$label != sub4$predictions, 1, 0)
    l4<- c(l4, sum(loss0==1)/length(loss0))
  }
  mcr=as.data.frame(rbind(l0,l1,l2,l3,l4,rating))
  mcr$Average=rowMeans(mcr)
  return(mcr)
}

#compute missclassification rate for the full model with wrist and chest
#Model 1: single effects
mod1= multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp + chest_Resp +
                 wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_BVP + wrist_EDA + wrist_Temp, data = df_train)
sum1=summary(mod1)
print(sum1)
save(sum1, file="Full.RData")

#Model 2: wo Chest Resp
mod2= multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp +
                 wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_BVP + wrist_EDA + wrist_Temp, data = df_train)
sum2=summary(mod2)
print(sum2)
save(sum2, file="woChestResp.RData")

#Model 3: wo Wrist BVP
mod3=multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp + chest_Resp +
                wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_EDA + wrist_Temp, data = df_train)
sum3=summary(mod3)
print(sum3)
save(sum3, file="woWristBVP.RData")

#Model 6: wo chest_Resp and wrist BVP
mod6=multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp  +
                wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z  + wrist_EDA + wrist_Temp, data = df_train)
sum6=summary(mod6)
print(sum6)
save(sum6, file="woChestRespwristBVP.RData")

#Get the MC Rates for the models
mcdat1=avclassifyrate(df, mod1)
mcdat2=avclassifyrate(df, mod2)
mcdat3=avclassifyrate(df, mod3)
mcdat6=avclassifyrate(df, mod6)

#Get the average MC rate for each model and save as a df
lab=c("MC Rate for Label=0","MC Rate for Label=1", "MC Rate for Label=2","MC Rate for Label=3","MC Rate for Label=4", "Overall MC Rate", "AIC")
mcav=as.data.frame(cbind(rowMeans(mcdat1), rowMeans(mcdat2), rowMeans(mcdat3), rowMeans(mcdat6)))

#bind AIC values to df
mcav=rbind(mcav, c(summary(mod1)$AIC,summary(mod2)$AIC,summary(mod3)$AIC,summary(mod6)$AIC))

rownames(mcav)=lab
colnames(mcav)=c("Average for M1", "Average for M2", "Average for M3", "Average for M6")

mcav
print(mcav)
save(mcav, file="MCAV_(backselect).RData")

#compare models using drop in deviance
#no interaction and two way
an1=as.data.frame(anova(mod2, mod1, test = "Chisq")) #check if 2 way interactions are significant
print(an1)
an1
an2=as.data.frame(anova(mod3, mod1, test = "Chisq")) #check if selected 3 way interactions are significant
print(an2)
an2
an5=as.data.frame(anova(mod6, mod1, test = "Chisq")) #check if selected interactions are significant
print(an5)
an5

save(an1, file="ANOVA_for_m1_vs_m2_(back).RData")
save(an2, file="ANOVA_for_m1_vs_m3_(back).RData")
save(an5, file="ANOVA_for_m1_vs_m6_(back).RData")