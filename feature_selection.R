corMatr <- cor(training[,num_predictors])
highlyCorrelated <- findCorrelation(corMatr, cutoff=.75)

boruta.bank_train <- Boruta(SalePrice~., data = training, doTrace = 2)
b <- boruta.bank_train$finalDecision
confirmed <- names(b[b == 'Confirmed'])
