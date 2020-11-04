# Correlated features - only works for numeric, can discard 3, not useful
corMatr <- cor(training[,num_predictors])
highlyCorrelated <- findCorrelation(corMatr, cutoff=.75)

# Boruta feature selection
boruta.train <- Boruta(SalePrice~., data = training, doTrace = 2)
b <- boruta.train$finalDecision
confirmed <- names(b[b == 'Confirmed'])

#Filter only confirmed features
trainB1 <- training[,c(confirmed, 'SalePrice')]
testB1 <- testing[,c(confirmed, 'SalePrice')]
submiB1 <- orig_submi[,confirmed]
