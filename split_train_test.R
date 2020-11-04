inTrain <- createDataPartition(orig_train$SalePrice, p=.75, list=FALSE)
inTrain <- rbind(inTrain, as.integer(rownames(orig_train[orig_train$Electrical=='legitNA',])))
training <- orig_train[inTrain,]
testing <- orig_train[-inTrain,]
