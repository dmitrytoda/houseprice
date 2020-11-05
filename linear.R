## Fit a model

# Silly linear regression
fitLR <- train(SalePrice ~ YrSold + MoSold + LotArea + BedroomAbvGr +
                       LotFrontage + Neighborhood, 
               data=training, method='lm')

# Linear regression based on boruta confirmed features
fitLR_B1 <- train(SalePrice ~ ., data=trainB1, method='lm')

## Predict on train and test sets

# Silly LR
pLR_train <- predict(fitLR, training[,-81])
pLR_test <- predict(fitLR, testing[,-81])

# Boruta confirmed LR
pLR_train_B1 <- predict(fitLR_B1, subset(trainB1, select=-c(SalePrice)))
pLR_test_B1 <- predict(fitLR_B1, subset(testB1, select=-c(SalePrice)))


## Calculate RMSE on train and test

# Silly LR
rmse(pLR_train, training[,81])
rmse(pLR_test, testing[,81])

# Boruta confirmed features only
rmse(pLR_train_B1, training[,81])
rmse(pLR_test_B1, testing[,81])

# Predict on submission set and create submission file
pLR_submi <- predict(fitLR, orig_submi)
pLR_submi <- predict(fitLR_B1, submiB1)

submit <- cbind(orig_submi$Id, pLR_submi)
colnames(submit) <- c("Id", 'SalePrice')
write.csv(submit, 'Nov4_silly_LR.csv', row.names = FALSE)
