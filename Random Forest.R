# basic linear model (biddy1 as y)
lin.model <- lm(biddy1~as.matrix(ebay2[,covariates]),data=ebay2,subset=-test1)
summary(lin.model)
# MSE
mean((ebay2$biddy1 - predict(lin.model,ebay2))[test1]^2)


# random forest(biddy1 as y)
for (i in 6:8){
  trainset1[,i] <- as.numeric(unlist(trainset1[,i]))
  testset1[,i] <- as.numeric(unlist(testset1[,i]))
  ebay2[,i] <- as.numeric(unlist(ebay2[,i]))
}

train_h2o <- as.h2o(trainset1)
test_h2o <- as.h2o(testset1)

rf1 <- h2o.randomForest(x=covariates,y=2,model_id = "rf1", seed=0,
                        training_frame = train_h2o,ntrees =100,nfolds=5)
rf1
# MSE
rffit1 <- h2o.performance(model=rf1,newdata=test_h2o)
rffit1
# variable importance
h2o.varimp(rf1)

##############################################################
# random forest (sell as y)
rf2 <- h2o.randomForest(x=covariates,y=1,model_id = "rf2", seed=0,
                        training_frame = train_h2o,ntrees =100,nfolds=5,
                        binomial_double_trees = TRUE)
rf2
# MSE
rffit2 <- h2o.performance(model=rf2,newdata=test_h2o)
rffit2
# accuracy of classification: 99.99%
rfhat2 <- h2o.predict(rf2,newdata=test_h2o)
rfhat2
rf_pred=rep(0,10000)
rf_pred[as.data.frame(rfhat2)>0.5]=1
table(rf_pred,testset1$sell)
mean(rf_pred==testset1$sell)
# variable importance
h2o.varimp(rf2)

######################################################
# find the highest bidding price

# basic linear model (biddy1 as y)
lin.model2 <- lm(biddy1~as.matrix(ebay3[,covar]),data=ebay3,subset=-test2)
summary(lin.model2)
# MSE
mean((ebay3$biddy1 - predict(lin.model2,ebay3))[test2]^2)


# random forest
for (i in 3:5){
  trainset2[,i] <- as.numeric(unlist(trainset2[,i]))
  testset2[,i] <- as.numeric(unlist(testset2[,i]))
  ebay3[,i] <- as.numeric(unlist(ebay3[,i]))
}
train2_h2o <- as.h2o(trainset2)
test2_h2o <- as.h2o(testset2)

rf3 <- h2o.randomForest(x=covar,y=2,model_id = "rf3", seed=0,
                        training_frame = train2_h2o,ntrees =100,nfolds=5)
rf3
# MSE
rffit3 <- h2o.performance(model=rf3,newdata=test2_h2o)
rffit3
# variable importance
h2o.varimp(rf3)
#####recommendation#####
biddy1_rf <- h2o.predict(object=rf3,newdata=as.h2o(recommendation))
biddy1_rf
recommendation$biddy1_rf <- as.data.frame(biddy1_rf)
# the highest 10 price
arrange(recommendation,desc(biddy1_rf))[1:10,]


######################################################
# find whether the car could be sold

# random forest (sell as y)
rf4 <- h2o.randomForest(x=covar,y=1,model_id = "rf4", seed=0,
                        training_frame = train2_h2o,ntrees =100,nfolds=5,
                        binomial_double_trees = TRUE)
rf4
# MSE
rffit4 <- h2o.performance(model=rf4,newdata=test2_h2o)
rffit4
# accuracy of classification: 95.2%
rfhat4 <- h2o.predict(rf4,newdata=test2_h2o)
rfhat4
rf_pred4=rep(0,2000)
rf_pred4[as.data.frame(rfhat4)>0.5]=1
table(rf_pred4,testset2$sell)
mean(rf_pred4==testset2$sell)
# variable importance
h2o.varimp(rf4)
#####recommendation#####
sell_rf <- h2o.predict(rf4,newdata=as.h2o(recommendation[,-12]))
recommendation$sell_rf <- ifelse(as.data.frame(sell_rf)>0.5,1,0)