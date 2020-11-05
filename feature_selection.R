# Correlated features - only works for numeric, can discard 3, not useful
corMatr <- cor(training[,num_predictors])
highlyCorrelated <- findCorrelation(corMatr, cutoff=.75)

# Boruta feature selection
boruta.train <- Boruta(SalePrice~., data = training, doTrace = 2, )
b <- boruta.train$finalDecision
confirmed <- names(b[b == 'Confirmed'])
tentative <- names(b[b == 'Tentative'])


#Filter only confirmed features
trainB1 <- training[,c(confirmed, tentative, 'SalePrice')]
testB1 <- testing[,c(confirmed, tentative, 'SalePrice')]
submiB1 <- orig_submi[,c(confirmed, tentative)]

train_all <- orig_train[,c(confirmed, tentative, 'SalePrice')]
