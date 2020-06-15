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

#final model
mod1= multinom(label ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp + chest_Resp + 
                 wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_BVP + wrist_EDA + wrist_Temp, data = df_train)


#random forest
rftrain=df_train
#single effects
rf <- randomForest(as.factor(label) ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp + chest_Resp + wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_BVP + wrist_EDA + wrist_Temp,data=rftrain, ntree=5000)

rftrain$rfpred=predict(rf, df_test)
#plot actual vs. predicted
jpeg(file="Actual vs. Predicted (RandFor)")
ggplot(data=rftrain, aes(rfpred, label))+geom_point()+xlab("Predicted")+ylab("Actual")+ggtitle("Actual vs. Predicted for Random Forest")
dev.off()

#conditional inteference tree
#single effects
ctmod <- ctree(as.factor(label) ~ chest_ACC_X + chest_ACC_Y + chest_ACC_Z + chest_ECG + chest_EMG + chest_EDA + chest_Temp + chest_Resp + wrist_ACC_X + wrist_ACC_Y + wrist_ACC_Z + wrist_BVP + wrist_EDA + wrist_Temp,data=rftrain)

rftrain$ctpred=predict(ctmod, df_test)
#plot actual vs. predicted
jpeg(file="Actual vs. Predicted (CondTree)")
ggplot(data=rftrain, aes(ctpred, label))+geom_point()+xlab("Predicted")+ylab("Actual")+ggtitle("Actual vs. Predicted for Conditional Inference Tree")
dev.off()

#actual predictions
rftrain$finpred=predict(mod1, df_test)

#Get missclass rate
avsens=as.data.frame(avclassifyrate(df, rf)$Average)
save(avsens, file="MisclassRandFor.RData")

#Make percentage classification table
pclass=merge(count(rftrain$finpred), count(rftrain$rfpred), by="x", all=T)
pclass=pclass[,-1]
pclass=(pclass/nrow(rftrain))*100
#Calculate differences in classification relative to final model
diffrf=ifelse(rftrain$finpred==rftrain$rfpred,0,1)
pclass=rbind(pclass, c(0,sum(diffrf)))
rownames(pclass)=c("% Classified as 0","% Classified as 1","% Classified as 2","% Classified as 3","% Classified as 4", "Absolute Diff in Classification")
colnames(pclass)=c("Final Model","Random Forest")
pclass[is.na(pclass)]=0
pclass
save(pclass, file="Classification_Table_for Forest.RData")