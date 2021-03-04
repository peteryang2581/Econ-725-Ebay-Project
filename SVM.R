# SVM (sell as y), might take 2 minutes to get the results
train1_svm <- trainset1[,c("sell",covariates)] 
test1_svm <- testset1[,c("sell",covariates)]
train1_svm$sell <- as.factor(train1_svm$sell)
test1_svm$sell <- as.factor(test1_svm$sell)

svmfit1 <- svm(sell~.,data=train1_svm)
summary(svmfit1)
svmpred1 <- predict(svmfit1,test1_svm[,-1])
svmpred1
# MSE
mean((as.numeric(unlist(test1_svm[,1]))-as.numeric(unlist(svmpred1)))^2)
# accuracy of classification: 99.36%
caret::confusionMatrix(svmpred1,test1_svm$sell)
agreement <- svmpred1 == test1_svm$sell
prop.table(table(agreement))
# variable importance
weights1 <- t(svmfit1$coefs) %*% svmfit1$SV           # weight vectors
weights1 <- apply(weights1, 2, function(v){sqrt(sum(v^2))}) %>%
  sort(decreasing=TRUE) 
weights1


# SVM (sell as y), might take 2 minutes to get the results
train2_svm <- trainset2[,c("sell",covar)]
test2_svm <- testset2[,c("sell",covar)]
train2_svm$sell <- as.factor(train2_svm$sell)
test2_svm$sell <- as.factor(test2_svm$sell)

svmfit2 <- svm(sell~.,data=train2_svm)
summary(svmfit2)
svmpred2 <- predict(svmfit2,test2_svm[,-1])
svmpred2
# MSE
mean((as.numeric(unlist(test2_svm[,1]))-as.numeric(unlist(svmpred2)))^2)
# accuracy of classification: 95.4% = 1-MSE
caret::confusionMatrix(svmpred2,test2_svm$sell)
agreement2 <- svmpred2 == test2_svm$sell
prop.table(table(agreement2))
# variable importance
weights2 <- t(svmfit2$coefs) %*% svmfit2$SV           # weight vectors
weights2 <- apply(weights2, 2, function(v){sqrt(sum(v^2))}) %>%
  sort(decreasing=TRUE) 
weights2
#####recommendation#####
sell_svm <- predict(svmfit2,recommendation[,covar])     # might take 30 seconds
recommendation$sell_svm <- as.data.frame(sell_svm)


### biddy*sell
for (i in 12:14){
  recommendation[,i] <- as.numeric(unlist(recommendation[,i]))
}

recommendation <- recommendation %>%
  mutate(sell_svm = sell_svm-1) %>%
  mutate(revenue_rf = biddy1_rf * sell_rf) %>%
  mutate(revenue_svm = biddy1_rf * sell_svm)
arrange(recommendation,desc(revenue_rf))[1:50,]
arrange(recommendation,desc(revenue_svm))[1:50,]