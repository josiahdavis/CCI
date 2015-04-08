#############################################
#                                           #
#         PARAMETER  OPTIMIZATION           #
#                                           #
#############################################

# ===========================================
#     Set up the workspace and get the data
# ===========================================

rm(list=ls()); gc()     # clear the workspace
set.seed(973487)        # Ensures you can repeat the results
library(rpart)          # For creating the tree
library(partykit)       # For plotting the tree
setwd("C:/Users/josdavis/Documents/Personal/GitHub/CCI")

# Get the data
data <- read.csv("titanic.csv", header = TRUE)
data$survived = data$survived == 'survived'
data <- na.omit(data)

# Split into training and testing sets
idxs <- runif(nrow(data)) < 0.7   # Random Indices
train <- data[idxs, ]             # Training set
test  <- data[!idxs, ]            # Testing set
rm(idxs)

# ===========================================
#     Create and evaluate the  model 
#     using default settings
# ===========================================

model <- rpart(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                data = train, 
                method = "class")

# Generate predictions (both probabilities and class predictions)
test$prediction <- predict(model, type = "prob", newdata = test)[,2] > 0.5

# Acccuracy in terms of classification rate (with 0.5 threshhold)
sum(test$prediction == test$survived) / nrow(test)

# ===========================================
#     Train multiple versions of the model
#     using the caret package
# ===========================================
library(caret)

# See here for a list of tuning parameters: http://topepo.github.io/caret/modelList.html

tc <- trainControl(method = "cv", number = 5)

tune_results <-train(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                   data = data, 
                   metric = "Accuracy",
                   method = "rpart2",
                   trainControl = tc,
                   tuneGrid = data.frame(maxdepth = c(1, 2, 3, 4, 5, 6, 7, 8)))

tune_results$bestTune

tune_model <- rpart(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                              data = train, 
                              method = "class",
                              control = rpart.control(cp = tune_results$bestTune))

# Generate predictions (both probabilities and class predictions)
test$prediction <- predict(tune_model, type = "prob", newdata = test)[,2] > 0.5

# Acccuracy in terms of classification rate (with 0.5 threshhold)
sum(test$prediction == test$survived) / nrow(test)