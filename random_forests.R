# ============================
#     Random Forest
# ============================


####
# Set up the workspace
####
rm(list=ls()); gc()     # clear the workspace
set.seed(973487)        # Ensures you can repeat the results
library(randomForest)   # For creating the Random Forests
setwd("C:/Users/josdavis/Documents/Personal/GitHub/CCI")

#####
# Get the data
####
data <- read.csv("titanic.csv", header = TRUE)
idxs <- runif(nrow(data)) < 0.7   # Random Indices
train <- data[idxs, ]             # Training set
test  <- data[!idxs, ]            # Testing set
summary(train)

#### 
# Run the tree
####

library(randomForest)
rf <- randomForest(form, train, importance = TRUE, na.action = na.omit)
sort(round(rf$importance[,4], 1), decreasing = TRUE) # Average decrease in the gini coefficient

pred.test.rf <- predict(rf, test)
round(prop.table(table(test$survived,pred.test.rf), 1)*100, 2)   # Correctly predicted from test set