#############################################
#
#               RANDOM FORESTS
#
#############################################


# ===========================================
#     Set up the workspace and get the data
# ===========================================

rm(list=ls()); gc()     # clear the workspace
set.seed(973487)        # Ensures you can repeat the results
library(randomForest)   # For creating the forest
setwd("C:/Users/josdavis/Documents/Personal/GitHub/CCI")

# Get the data
data <- read.csv("titanic.csv", header = TRUE)
data$survived = data$survived == 'survived'

# Split into training and testing sets
idxs <- runif(nrow(data)) < 0.7   # Random Indices
train <- data[idxs, ]             # Training set
test  <- data[!idxs, ]            # Testing set
summary(train)


# ===========================================
#       Run the forest
# ===========================================
rf <- randomForest(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                   data = train,
                   na.action = na.omit)

# Show the values of the random forest
rf

# Print out the importance scores
rf$importance

# ===========================================
#       Control the parameters of the forest
# ===========================================

# Change the number of variables considered for each split
rf <- randomForest(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                   data = train,
                   na.action = na.omit,
                   mtry = 3)

# Change the number of trees
rf <- randomForest(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                   data = train,
                   na.action = na.omit,
                   ntree = 300)

# ===========================================
#       Evaluate the accuracy of the forest
# ===========================================

# Generate predictions (both probabilities and class predictions)
test$predict_proba <- predict(rf, test)[,2]
test$prediction <- predict_proba > 0.5

# Acccuracy in terms of classification rate (with 0.5 threshhold)
sum(test$prediction == test$survived) / nrow(test)

# Confusion Matrix (rows are predictions, colums are actuals)
table(test$prediction, test$survived)
prop.table(table(test$prediction, test$survived), 2)

# Sensitivity: When the person survived, how often did it predict survival?
# A.K.A. True Positive Rate
test_lived = test[test$survived,]
sum(test_lived$prediction == test_lived$survived) / nrow(test_lived)

# Specificty: When the person died, how often did it predict death?
# A.K.A. True Negative Rate
test_died = test[!test$survived,]
sum(test_died$prediction == test_died$survived) / nrow(test_died)