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

# See here for a list of tuning parameters: http://topepo.github.io/caret/modelList.html
tuned_tree <-train(survived ~ pclass + sex + age + sibsp + parch, 
                     data = train,
                     method = "rpart2")

# Print out the best parameter option
tuned_tree$bestTune

# Print out the results for all values
tuned_tree$results

# Print out a summary of the results
tuned_tree

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
sum(preds == test$survived) / nrow(test)

# The confusionMatrix shows the predicted and actual values
confusionMatrix(data = preds, test$survived)

# ===========================================
#   Controling the tuning sequence
# ===========================================

# tuneLength specifies HOW MANY OPTIONS the tuning parameter will be set to  
tuned_tree <-train(survived ~ pclass + sex + age + sibsp + parch, 
                   data = train,
                   method = "rpart2",
                   tuneLength = 5)

tuned_tree$results
plot(tuned_tree)

# tuneGrid specifies THE ACTUAL OPTIONS the tuning parameter will be set to
tuned_tree <-train(survived ~ pclass + sex + age + sibsp + parch, 
                   data = train,
                   method = "rpart2",
                   tuneGrid = data.frame(maxdepth = c(2, 4, 6, 8, 10, 12)))

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
                     method = "rpart2",
                     trControl = tc)

# Note the resampling method
tuned_tree

# Plot the results of the tuning
plot(tuned_tree)


# --- TO-DO ---
# Add example of a two-dimensional tuning grid
# e.g. expand.grid(mfinal = c(1, 5, 9), max_depth = (1:3))
# Add example of pre-processing


# The data can be pre-processed within the train function call
tuned_tree <-train(survived ~ pclass + sex + age + sibsp + parch, 
                   data = train,
                   method = "rpart2",
                   trControl = tc,
                   preProc = c("center", "scale"))