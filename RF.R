fitRF <- train(SalePrice ~ YrSold + MoSold + LotArea + BedroomAbvGr +
                                LotFrontage + Neighborhood, 
                        data=training, method='rf')

# Predict on train and test sets
pRF_train <- predict(fitRF, training[,-81])
pRF_test <- predict(fitRF, testing[,-81])


# Calculate RMSE on train and test
rmse(pRF_train, training[,81])
rmse(pRF_test, testing[,81])

# Predict on submission set and create submission file
pRF_submi <- predict(fitRF, orig_submi)
submit <- cbind(orig_submi$Id, pRF_submi)
colnames(submit) <- c("Id", 'SalePrice')
write.csv(submit, 'Nov 1 RF.csv', row.names = FALSE)
