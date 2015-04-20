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
idxs <- runif(nrow(data)) < 0.75   # Random Indices
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

# See here for a list of supported method arguments: http://topepo.github.io/caret/modelList.html

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
                      tuneGrid = data.frame(maxdepth = seq(1, 5)))

tuned_tree$results
plot(tuned_tree)

# ===========================================
#   EXERCISE: Tuning the Random Forest
# ===========================================

# 1) Read in the Salaries dataset and split into train/test sets


# 2) Create a fitted tuning object for a Random Forest called 'tuned_forest' with default 
#    settings for the tuning grid and a 3-fold cross-validation.


# 3) Determine the optimal parameter value for tuned_forest with the default tuning grid.
#    What does the tuning parameter represent for the Random Forest?



# 4) Specify your own tuning grid for the random forest.


# 5) Create a new fitted tuning object for the Random Forest using your own tuning grid.


# 6) Create predictions with the fitted model on the test dataset


# 7) Evaluate the accuracy on the test dataset




# ===========================================
#   SOLUTIONS: Tuning the Random Forest
# ===========================================


# 1) Read in the Salaries dataset and split into train/test sets
data <- read.csv("salaries.csv", header = TRUE)
data <- na.omit(data)

idxs <- runif(nrow(data)) < 0.75   # Random Indices
train <- data[idxs, ]             # Training set
test  <- data[!idxs, ]            # Testing set
rm(idxs, data)

# 2) Create a fitted tuning object for a Random Forest called 'tuned_forest' with default 
#    settings for the tuning grid and a 3-fold cross-validation.

tuned_forest <-train(salary ~ ., 
                     data = train,
                     method = "rf",
                     trControl = trainControl(method = "cv", number = 3))

# 3) Determine the optimal parameter value for tuned_forest with the default tuning grid.
#    What does the tuning parameter represent for the Random Forest?

tuned_forest$bestTune
ggplot(tuned_forest)

# 4) Specify your own tuning grid for the random forest.
tg <- data.frame(mtry = seq(1, 5))

# 5) Create a new fitted tuning object for the Random Forest using your own tuning grid.
tuned_forest <-train(survived ~ pclass + sex + age + sibsp + parch, 
                     data = train,
                     method = "rf",
                     trControl = trainControl(method = "cv", number = 3),
                     tuneGrid = tg)

# 6) Create predictions with the fitted model on the test dataset
pred_rf <- predict(tuned_forest, test, type = "raw")

# 7) Evaluate the accuracy on the test dataset
sum(pred_rf == test$survived) / nrow(test)


# ===========================================
#   NOTES
# ===========================================

# ---- Not covered ----
# Imputation of missing data
# Pre-processing of data (e.g., center and scaling)
# Tuning over multiple parameters (not supported in many packages)

# ---- Going further ----
# The author of this package wrote an excellent book: Applied Predictive Modeling http://appliedpredictivemodeling.com/
# Additional Documentation on training and tuning: http://topepo.github.io/caret/training.html

# ---- Gender Model ----
# Female passengers have much higher survival rates than Males
summarise(group_by(train, sex), survival_rate = mean(survived == 'survived'))

# Make the predictions based solely on gender
preds = ifelse(train$sex == 'female', 'survived', 'died')

# Evaluate the accuracy of the gender only model 
sum(preds == train$survived) / nrow(train) # 0.7852