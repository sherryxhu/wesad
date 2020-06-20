install.packages('class', repos = "http://cran.us.r-project.org")


library(class)

load("data/df_train.RData")
load("data/df_test.RData")


df_train_partial = df_train
df_test_partial = df_test
res = knn(df_train[,3:ncol(df_train)], df_test[,3:ncol(df_test)], df_train$label, k=floor(sqrt(nrow(df_train))))
accuracy <- 100 * sum(df_test[,1] == res)/NROW(df_test)
