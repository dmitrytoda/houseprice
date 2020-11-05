fitRF <- train(SalePrice ~ YrSold + MoSold + LotArea + BedroomAbvGr +
                                LotFrontage + Neighborhood, 
                        data=training, method='rf')

fitRF_B <- train(SalePrice ~ ., data=trainB1, method='rf')
fitRF_all <- train(SalePrice ~ ., data=train_all, method='rf')

# Predict on train and test sets
pRF_train <- predict(fitRF_B, training[,-81])
pRF_test <- predict(fitRF_B, testing[,-81])


# Calculate RMSE on train and test
rmse(pRF_train, training[,81])
rmse(pRF_test, testing[,81])

# Predict on submission set and create submission file
pRF_submi <- predict(fitRF_all, orig_submi)
submit <- cbind(orig_submi$Id, pRF_submi)
colnames(submit) <- c("Id", 'SalePrice')
write.csv(submit, 'Nov4_RF_Boruta_all.csv', row.names = FALSE)
