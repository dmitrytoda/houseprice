for (i in names(categorics)) {
        for (j in levels(categorics[,i]))
                if (sum(categorics[,i]==j) <= 10) {
                        affected_features <- c(affected_features, i)
                        affected_features[i] <- ifelse(is.na(affected_features[i]),1,as.integer(affected_features[i])+1)
                        categorics[categorics[,i]==j, i] <- names(sort(-table(categorics[,i])))[1]
                }
}