###############################################
###                                         ###
###         PARAMETER OPTIMIZATION          ###
###                                         ###
###############################################

# ===========================================
#   Set up the workspace and get the data
# ===========================================

rm(list=ls()); gc()     # clear the workspace
set.seed(973487)        # Ensures you can repeat the results
setwd("C:/Users/josdavis/Documents/Personal/GitHub/CCI")

# Get the data
data <- read.csv("titanic.csv", header = TRUE)
data <- na.omit(data)

# Split into training and testing sets
idxs <- runif(nrow(data)) < 0.8   # Random Indices
train <- data[idxs, ]             # Training set
test  <- data[!idxs, ]            # Testing set
rm(idxs, data)

# ===========================================
#   Run and evaluate the tuning sequence
# ===========================================
library(caret)

# Create a fitted tuning object
tuned_tree <-train(survived ~ pclass + sex + age + sibsp + parch,
                   data = train,                            # data refers to the dataset used for tuning
                   method = "rpart2",                       # method refers to the 
                   trControl = trainControl(method = "cv")) # trainControl refers to the resampling details


# See here for a list of tuning parameters: http://topepo.github.io/caret/modelList.html

# Print out a summary of the results
tuned_tree

# Print out the model evaluation results only
tuned_tree$results

# Print out the best parameter option
tuned_tree$bestTune

# Plot the results of the tuning
plot(tuned_tree$results$maxdepth, tuned_tree$results$Accuracy)

# Alternatively, caret has a built in plotting option
plot(tuned_tree)
ggplot(tuned_tree)

# ===========================================
#   Making and evaluating predictions 
# ===========================================

# The output of the training process is a model with the "optimal" parameters
# This model can be used to make predictions on out of sample data

# type = "raw" gives class output,  
pred <- predict(tuned_tree, test, type = "raw")

# type = "prob" gives probabilities
pred_proba <- predict(tuned_tree, test, type = "prob")

# Evaluate the accuracy of the "optimal" model
sum(pred == test$survived) / nrow(test)

# The confusionMatrix shows the predicted and actual values
confusionMatrix(data = pred, test$survived)
# See ?confusionMatrix for a reminder on the formulas for these measures

# ===========================================
#   Modifying the tuning sequence
# ===========================================

# tuneLength specifies HOW MANY OPTIONS the tuning parameter will be set to  
tuned_tree <-train(survived ~ pclass + sex + age + sibsp + parch, 
                   data = train,
                   method = "rpart2",
                   trControl = trainControl(method = "cv"),
                   tuneLength = 5)

# NOTE: 0.785 is the accuracy if you predict based solely on train$sex

tuned_tree$results
plot(tuned_tree)

# tuneGrid specifies THE ACTUAL OPTIONS the tuning parameter will be set to
tuned_tree <-train(survived ~ pclass + sex + age + sibsp + parch, 
                   data = train,
                   method = "rpart2",
                   trControl = trainControl(method = "cv"),
                   tuneGrid = data.frame(maxdepth = c(1, 2, 3, 4, 5)))

tuned_tree$results
plot(tuned_tree)

# method specifies the model and default tuning parameters  
tuned_forest <-train(survived ~ pclass + sex + age + sibsp + parch, 
                     data = train,
                     method = "rf")

tuned_forest

# See here for a complete list of supported method options: 
# http://topepo.github.io/caret/modelList.html

# ===========================================
#   Controlling the resampling process
# ===========================================

# the trainControl function specifies the nuances 
# of the resampling process.

tc <- trainControl(
                   # "cv" stands for cross-validation
                   method = "cv", 
                   
                   # The number of folds
                   number = 5)

tuned_tree <-train(survived ~ pclass + sex + age + sibsp + parch, 
                     data = train,
                     method = "rpart2", #aasdad
                     trControl = tc)

# Note the resampling method
tuned_tree

# Plot the results of the tuning
plot(tuned_tree)

# ===========================================
#   Notes
# ===========================================

# ---- Not covered ----
# Imputation of missing data
# Pre-processing of data (e.g., center and scaling)
# Tuning over multiple parameters (not supported in many packages)

# ---- Going further ----
# The author of this package wrote an excellent book: Applied Predictive Modeling
# http://appliedpredictivemodeling.com/

# ---- Gender Model ----
# Female passengers have much higher survival rates than Males
summarise(group_by(train, sex), survival_rate = mean(survived == 'survived'))

# Make the predictions based solely on gender
preds = ifelse(train$sex == 'female', 'survived', 'died')

# Evaluate the accuracy of the gender only model
sum(preds == train$survived) / nrow(train)