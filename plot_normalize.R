# SalePrice looks skewed to the left (positive skew)
ggplot(data = orig_train, aes(SalePrice)) +
        geom_histogram(aes(y=..density..),fill='steelblue') +
        geom_density(col=2)

# Scatter plots of SalePrice ~ all numeric predictors

for (i in num_predictors) {
        jpeg(paste("./scatter/",i, '.jpeg'))
        g <- ggplot(data=orig_train, 
                    aes(x=orig_train[,i], y=SalePrice, 
                        col=!Outlier)
                    ) +
                geom_point(alpha=.5) + xlab(i)
        print(g)
        dev.off()
}


# Mark outliers based on SalePrice, LotFrontage, GrLivArea

orig_train %>% select(Id, SalePrice) %>% 
        arrange(desc(SalePrice)) %>% head()

orig_train$Outlier <- FALSE

orig_train[c(1299, 524, 935, 692, 1183),'Outlier'] <- TRUE

# Remove outliers (and the extra column)

training <- orig_train[-c(1299, 524, 935, 692, 1183),]
training$Outlier <- NULL

# Scatter plots of SalePrice ~ factor predictors - not much help
for (i in fact_predictors) {
        jpeg(paste("./scatter-factor/",i, '.jpeg'))
        g <- ggplot(data=orig_train, 
                    aes(x=orig_train[,i], y=SalePrice, 
                        col=!Outlier)
        ) +
                geom_point(alpha=.5) + xlab(i)
        print(g)
        dev.off()
}

# Find skewed columns

# Original columns 
for (i in num_predictors) {
        if(skewness(orig_train[,i]) >.5) 
                print(paste(i, skew(orig_train[,i])))        
}

# Apply log(1+x) - better but still not ideal
for (i in num_predictors) {
        if(skewness(log(1+orig_train[,i])) >.5) 
                print(paste(i, skew(log(1+orig_train[,i]))))        
}

# Box-Cox








