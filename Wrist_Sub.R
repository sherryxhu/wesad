# install packages
install.packages('nnet', repos = "http://cran.us.r-project.org")
install.packages('tidyverse', repos = "http://cran.us.r-project.org")
install.packages('dplyr', repos = "http://cran.us.r-project.org")
install.packages('arm', repos = "http://cran.us.r-project.org")
install.packages('plyr', repos = "http://cran.us.r-project.org")
install.packages('randomForest', repos = "http://cran.us.r-project.org")
install.packages('caTools', repos = "http://cran.us.r-project.org")
install.packages('partykit', repos = "http://cran.us.r-project.org")
install.packages('jpeg', repos = "http://cran.us.r-project.org")

# load packages
library(tidyverse)
library(dplyr)
library(nnet) # for multinom
library(plyr) # for count
library(knitr) # for kable
library(randomForest) # for random forest
library(caTools) # for random forest
library(partykit) # for ctree
library(jpeg)

#load data
load("../data/df.RData")
load("../data/df_train.RData")
load("../data/df_test.RData")

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

# Check submodels for just wrist predictors
mcr=NULL
mcdat=NULL

#Model 1: single effects
m1 <- multinom(label ~ (wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_BVP + wrist_EDA + wrist_Temp), data = df_train)
sum1=summary(m1)
print(sum1)
save(sum1, file="WristMod1.RData")

#Model 2: single+wrist_ACC_X:wrist_ACC_Y:wrist_ACC_Z + wrist_EDA:wrist_Temp + wrist_BVP:wrist_Temp+ wrist_BVP:wrist_EDA
m2 <- multinom(label ~ wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_BVP + wrist_EDA + wrist_Temp + wrist_ACC_X:wrist_ACC_Y:wrist_ACC_Z + wrist_EDA:wrist_Temp + wrist_BVP:wrist_Temp+ wrist_BVP:wrist_EDA, data = df_train)
sum2=summary(m2)
print(sum2)
save(sum2, file="WristMod2.RData")

#Model 3: single+Time
m3 <- multinom(label ~ (wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_BVP + wrist_EDA + wrist_Temp+Time), data = df_train)
sum3=summary(m3)
print(sum3)
save(sum3, file="WristMod1.RData")

#get the AIC for eachh model
modaic=c(summary(m1)$AIC,summary(m2)$AIC,summary(m3)$AIC)

# column of predicted classes
df_test$predm1 <- predict(m1, df_test)
#plot Actual vs. Predicted
jpeg(file="Actual vs. Predicted (Wrist Submodel 1)")
ggplot(data=df_test, aes( predm1, label))+geom_point()+xlab("Predicted")+ylab("Actual")+ggtitle("Actual vs. Predicted for Model 1")
dev.off()

# column of predicted classes
df_test$predm2 <- predict(m2, df_test)
#plot Actual vs. Predicted
jpeg(file="Actual vs. Predicted (Wrist Submodel 2)")
ggplot(data=df_test, aes(predm2, label))+geom_point()+xlab("Predicted")+ylab("Actual")+ggtitle("Actual vs. Predicted for Model 2")
dev.off()

# column of predicted classes
df_test$predm3 <- predict(m3, df_test)
#plot Actual vs. Predicted
jpeg(file="Actual vs. Predicted (Wrist Submodel 3)")
ggplot(data=df_test, aes(predm3, label))+geom_point()+xlab("Predicted")+ylab("Actual")+ggtitle("Actual vs. Predicted for Model 3")
dev.off()


#Get the MC Rates for the models
m1rate=avclassifyrate(df, m1)
m2rate=avclassifyrate(df, m2)
m3rate=avclassifyrate(df, m3)


#Get the average MC rate for each model and save as a df
mcdat=as.data.frame(cbind(m1rate$Average, m2rate$Average, m3rate$Average,m4rate$Average, m5rate$Average))
mcdat=rbind(mcdat, modaic)
lab=c("MC Rate for Label=0","MC Rate for Label=1", "MC Rate for Label=2","MC Rate for Label=3","MC Rate for Label=4", "Overall MC Rate", "AIC")
rownames(mcdat)=lab
colnames(mcdat)=c("Model 1", "Model 2", "Model 3", "Model4", "Model 5")

mcdat
save(mcdat, file="MCAV_(comparing_wristsubs).RData")