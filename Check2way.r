# install packages
install.packages('nnet', repos = "http://cran.us.r-project.org")
install.packages('tidyverse', repos = "http://cran.us.r-project.org")
install.packages('dplyr', repos = "http://cran.us.r-project.org")
install.packages('arm', repos = "http://cran.us.r-project.org")
install.packages('plyr', repos = "http://cran.us.r-project.org")
install.packages('randomForest', repos = "http://cran.us.r-project.org")
install.packages('caTools', repos = "http://cran.us.r-project.org")
install.packages('partykit', repos = "http://cran.us.r-project.org")
install.packages('caret', repos = "http://cran.us.r-project.org")

# load packages
library(tidyverse)
library(dplyr)
library(nnet) # for multinom
library(plyr) # for count
library(knitr) # for kable
library(randomForest) # for random forest
library(caTools) # for random forest
library(partykit) # for ctree
library(caret) #confusion matrix

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
mod1= multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp +
                 wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_EDA + wrist_Temp, data = df_train)
sum1=summary(mod1)
print(sum1)
save(sum1, file="CompMod1.RData")

#Model 2: single effects + chest_Temp:chest_EDA + chest_ECG:chest_Resp + wrist_EDA:wrist_Temp + wrist_BVP:wrist_Temp+ wrist_BVP:wrist_EDA
mod2=multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp  +
                wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_EDA + wrist_Temp + chest_Temp:chest_EDA + wrist_EDA:wrist_Temp, data = df_train)
sum2=summary(mod2)
print(sum2)
save(sum2, file="CompMod2.RData")

#Model 3: single effects + chest_Temp:chest_EDA + chest_ECG:chest_Resp + chest_EMG:chest_EDA + wrist_EDA:wrist_Temp + wrist_BVP:wrist_Temp+ wrist_BVP:wrist_EDA
mod3=multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp +
                wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_EDA + wrist_Temp+ chest_Temp:chest_EDA + chest_EMG:chest_EDA + wrist_EDA:wrist_Temp, data = df_train)
sum3=summary(mod3)
print(sum3)
save(sum3, file="CompMod3.RData")


#Model 4: single effects + wrist_ACC_X:wrist_ACC_Y:wrist_ACC_Z + chest_ACC_X:chest_ACC_Y:chest_ACC_Z + chest_Temp:chest_EDA + chest_ECG:chest_Resp + chest_EMG:chest_EDA + wrist_EDA:wrist_Temp + wrist_BVP:wrist_Temp+ wrist_BVP:wrist_EDA
mod4=multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp +
                wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_EDA + wrist_Temp + wrist_ACC_X:wrist_ACC_Y:wrist_ACC_Z + chest_ACC_X:chest_ACC_Y:chest_ACC_Z + chest_Temp:chest_EDA + chest_EMG:chest_EDA + wrist_EDA:wrist_Temp, data = df_train)
sum4=summary(mod4)
print(sum4)
save(sum4, file="CompMod4.RData")

#Model 5
mod5=multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp +
                wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_EDA + wrist_Temp + wrist_ACC_X:wrist_ACC_Y:wrist_ACC_Z + chest_ACC_X:chest_ACC_Y:chest_ACC_Z + chest_Temp:chest_EDA + wrist_EDA:wrist_Temp, data = df_train)

sum5=summary(mod5)
print(sum5)
save(sum5, file="CompMod5.RData")


#Get the MC Rates for the models
mcdat1=avclassifyrate(df, mod1)
mcdat2=avclassifyrate(df, mod2)
mcdat3=avclassifyrate(df, mod3)
mcdat4=avclassifyrate(df, mod4)
mcdat5=avclassifyrate(df, mod5)


#Get the average MC rate for each model and save as a df
lab=c("MC Rate for Label=0","MC Rate for Label=1", "MC Rate for Label=2","MC Rate for Label=3","MC Rate for Label=4", "Overall MC Rate")

mcav=as.data.frame(cbind(rowMeans(mcdat3), rowMeans(mcdat4)))

#bind AIC values to df
mcav=rbind(mcav, c(sum1$AIC,sum2$AIC,sum3$AIC,sum4$AIC, sum5$AIC))

rownames(mcav)=lab
colnames(mcav)=c("Average for M1", "Average for M2", "Average for M3", "Average for M4", "Average for M5")

mcav
print(mcav)
save(mcav, file="MCAV_intcomp.RData")

mod1pred = predict(mod1, df_test)
print(confusionMatrix(mod1pred, df_test$label))

mod2pred = predict(mod2, df_test)
print(confusionMatrix(mod2pred, df_test$label))

mod3pred = predict(mod3, df_test)
print(confusionMatrix(mod3pred, df_test$label))

mod4pred = predict(mod4, df_test)
print(confusionMatrix(mod4pred, df_test$label))


mod5pred = predict(mod5, df_test)
print(confusionMatrix(mod5pred, df_test$label))

#compare models using drop in deviance
#no interaction and two way
an1=as.data.frame(anova(mod1, mod2, test = "Chisq")) #check if 2 way interactions are significant
an1
an2=as.data.frame(anova(mod1, mod3, test = "Chisq")) #check if selected 3 way interactions are significant
an2
an3=as.data.frame(anova(mod1, mod4, test = "Chisq")) #check if selected interactions are significant
an3
an4=as.data.frame(anova(mod1, mod5, test = "Chisq")) #check if selected interactions are significant
an4


save(an1, file="ANOVAm12way1.RData")
save(an2, file="ANOVAm22way2.RData")
save(an3, file="ANOVAm23way.RData")
save(an4, file="ANOVAm23way2.RData")

