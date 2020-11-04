inTrain <- createDataPartition(orig_train$SalePrice, p=.75, list=FALSE)
training <- orig_train[inTrain,]
testing <- orig_train[-inTrain,]
