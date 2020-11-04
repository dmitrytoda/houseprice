# Fit a model
fitLR <- train(SalePrice ~ YrSold + MoSold + LotArea + BedroomAbvGr +
                       LotFrontage + Neighborhood, 
               data=training, method='lm')

# Predict on train and test sets
pLR_train <- predict(fitLR, training[,-81])
pLR_test <- predict(fitLR, testing[,-81])
# pLR_test[pLR_test < 0] <- 1


# Calculate RMSE on train and test
rmse(pLR_train, training[,81])
rmse(pLR_test, testing[,81])

# Predict on submission set and create submission file
pLR_submi <- predict(fitLR, orig_submi)
submit <- cbind(orig_submi$Id, pLR_submi)
colnames(submit) <- c("Id", 'SalePrice')
write.csv(submit, 'Nov 1.csv', row.names = FALSE)