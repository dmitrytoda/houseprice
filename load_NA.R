options(scipen=999)
library(ggplot2)
library(lubridate)
library(caret)
library(dplyr)
library(Boruta)
set.seed(1101)

orig_train <- read.csv('train.csv', na.strings = '')
orig_submi <- read.csv('test.csv', na.strings = '')

# NA_train <- sapply(orig_train, function(x) sum(is.na(x)))
# sort(NA_train[NA_train != 0], decreasing=TRUE)
# 
# NA_submi <- sapply(orig_submi, function(x) sum(is.na(x)))
# sort(NA_submi[NA_submi != 0], decreasing=TRUE)

rmse <- function(x,y) {
        sqrt(mean( (log(x)-log(y))^2 ))
}

# LotFrontage, MasVnrArea                               : factor -> int
orig_train$LotFrontage <- as.integer(orig_train$LotFrontage)
orig_train$MasVnrArea <- as.integer(orig_train$MasVnrArea)
orig_submi$LotFrontage <- as.integer(orig_submi$LotFrontage)
orig_submi$MasVnrArea <- as.integer(orig_submi$MasVnrArea)

# GarageYrBlt, MoSold, YrSold, YearBuilt, YearRemodAdd  : int -> date

num_predictors <- colnames(select_if(orig_train, is.numeric))
fact_predictors <- colnames(orig_train)
fact_predictors <- fact_predictors[!fact_predictors %in% num_predictors]
num_predictors <- num_predictors[!num_predictors %in% c('Id','SalePrice')]
