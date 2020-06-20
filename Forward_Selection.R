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
mod4=multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp  +
                wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_EDA + wrist_Temp + chest_Temp:chest_EDA, data = df_train)
sum4=summary(mod4)
print(sum4)
save(sum4, file="CompMod4.RData")

#compare models using drop in deviance
#no interaction and two way
an1=as.data.frame(anova(mod1, mod4, test = "Chisq")) #check if 2 way interactions are significant
an1
an2=as.data.frame(anova(mod4, mod2, test = "Chisq")) #check if selected 3 way interactions are significant
an2
an3=as.data.frame(anova(mod2, mod3, test = "Chisq")) #check if selected interactions are significant
an3


#save(an1, file="ANOVAm12way1.RData")
#save(an2, file="ANOVAm22way2.RData")
#save(an3, file="ANOVAm23way.RData")
#save(an4, file="ANOVAm23way2.RData")

