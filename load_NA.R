options(scipen=999)
library(ggplot2)
library(lubridate)
library(caret)
library(dplyr)
library(Boruta)
set.seed(1101)

orig_train <- read.csv('train.csv') #, na.strings = '')
orig_submi <- read.csv('test.csv')  #, na.strings = '')

rmse <- function(x,y) {
        sqrt(mean( (log(x)-log(y))^2 ))
}

# LotFrontage, MasVnrArea, GarageYrBlt     : factor -> int
# Need to do this only if loading with na.strings=''
# orig_train$LotFrontage <- as.integer(as.character(orig_train$LotFrontage))
# orig_train$MasVnrArea <- as.integer(as.character(orig_train$MasVnrArea))
# orig_train$GarageYrBlt <- as.integer(as.character(orig_train$GarageYrBlt))
# 
# orig_submi$LotFrontage <- as.integer(as.character(orig_submi$LotFrontage))
# orig_submi$MasVnrArea <- as.integer(as.character(orig_submi$MasVnrArea))
# orig_submi$GarageYrBlt <- as.integer(as.character(orig_submi$GarageYrBlt))



# MoSold, YrSold, YearBuilt, YearRemodAdd  : int -> date

# num_predictors <- colnames(select_if(orig_train, is.numeric))
# fact_predictors <- colnames(orig_train)
# fact_predictors <- fact_predictors[!fact_predictors %in% num_predictors]
# num_predictors <- num_predictors[!num_predictors %in% c('Id','SalePrice')]

############################################################

## Dealing with NA values
## Many columns have 'NA' values, some of them are missing but some are legit levels (no alley access, no garage etc)

# Find all the NA
NA_train <- sapply(orig_train, function(x) sum(is.na(x)))
sort(NA_train[NA_train != 0], decreasing=TRUE)

NA_submi <- sapply(orig_submi, function(x) sum(is.na(x)))
sort(NA_submi[NA_submi != 0], decreasing=TRUE)

# These 14 columns have legit 'NA' levels: 
# Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2
# FireplaceQu, GarageType, GarageFinish, GarageQual, GarageCond
# PoolQC, Fence, MiscFeature

# Transform NA to "NA" for the 14 columns
legitNA <- c('Alley', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 
             'BsmtFinType2', 'FireplaceQu', 'GarageType', 'GarageFinish', 
             'GarageQual', 'GarageCond', 'PoolQC', 'Fence', 'MiscFeature')

# Add a new level to all factor columns
addLegitNA <- function(x){
        if(is.factor(x)) return(factor(x, levels=c(levels(x), "legitNA")))
        return(x)
}
        
orig_train <- as.data.frame(lapply(orig_train, addLegitNA))
orig_submi <- as.data.frame(lapply(orig_submi, addLegitNA))

orig_train[,legitNA] <- apply(orig_train[,legitNA], 2, function(x){ifelse(is.na(x),'legitNA', x)})
orig_submi[,legitNA] <- apply(orig_submi[,legitNA], 2, function(x){ifelse(is.na(x),'legitNA', x)})

# In some of the remaining columns, NA can also be replaces with 0 (it means no garage etc)
zeroNA <- c('LotFrontage', 'MasVnrArea', 'BsmtFullBath', 'BsmtHalfBath',
            'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF', 'GarageCars',
            'GarageArea')
orig_train[,zeroNA] <- apply(orig_train[,zeroNA], 2, function(x){ifelse(is.na(x),0, x)})
orig_submi[,zeroNA] <- apply(orig_submi[,zeroNA], 2, function(x){ifelse(is.na(x),0, x)})

# There are still a few remaining NAs in both training and submission sets
# Let's deal with them manually _somehow_, maybe not the best approach but 
# we can always revisit if it doesn't work very well (not a lot of them anyway)

# GarageYrBlt : set to house built year
orig_train$GarageYrBlt <- sapply(
        orig_train$GarageYrBlt,
        function(x) ifelse(is.na(x),orig_train$YearBuilt,x))
orig_submi$GarageYrBlt <- sapply(
        orig_submi$GarageYrBlt,
        function(x) ifelse(is.na(x),orig_submi$YearBuilt,x))

# Electrical, Utilities : set to 'legitNA'
legitNA2 <- c('Electrical', 'Utilities')
orig_train[,legitNA2] <- apply(orig_train[,legitNA2], 2, function(x){ifelse(is.na(x),'legitNA', x)})
orig_submi[,legitNA2] <- apply(orig_submi[,legitNA2], 2, function(x){ifelse(is.na(x),'legitNA', x)})

# MasVnrType : set to "None" (there is already such level)
# need to use dplyr::if_else cuz normal ifelse replaces factor values with numerics
# (above with more than 1 column no such problem BTW)
orig_train$MasVnrType <- sapply(orig_train$MasVnrType,function(x) if_else(is.na(x), 'None', as.character(x)))
orig_submi$MasVnrType <- sapply(orig_submi$MasVnrType,function(x) if_else(is.na(x), 'None', as.character(x)))

# MSZoning, Functional, Exterior1st, Exterior2nd, KitchenQual, SaleType : need to impute
# As zeroth approximation, let's fill them with the most frequent values
orig_submi$MSZoning <- sapply(orig_submi$MSZoning,function(x) if_else(is.na(x),'RL',as.character(x)))
orig_submi$Exterior1st <- sapply(orig_submi$Exterior1st,function(x) if_else(is.na(x),'VinylSd',as.character(x)))
orig_submi$Exterior2nd <- sapply(orig_submi$Exterior2nd,function(x) if_else(is.na(x),'VinylSd',as.character(x)))
orig_submi$KitchenQual <- sapply(orig_submi$KitchenQual,function(x) if_else(is.na(x),'TA',as.character(x)))
orig_submi$Functional <- sapply(orig_submi$Functional,function(x) if_else(is.na(x),'Typ',as.character(x)))
orig_submi$SaleType <- sapply(orig_submi$SaleType,function(x) if_else(is.na(x),'WD',as.character(x)))

