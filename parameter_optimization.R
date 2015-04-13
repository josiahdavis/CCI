###############################################
###                                         ###
###         PARAMETER OPTIMIZATION          ###
###                                         ###
###############################################

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

# ===========================================
#     Run the tuning sequence
# ===========================================

library(caret)
# See here for a list of tuning parameters: http://topepo.github.io/caret/modelList.html

tune_results <-train(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                     data = data2,
                     method = "rpart2")


# Print out the best parameter option
tune_results$bestTune

# Print out the results for all values
tune_results$results

# Plot the results of the tuning
plot(tune_results$results$maxdepth, tune_results$results$Accuracy)

# Print out a summary of the results
tune_results

# ===========================================
#     Control parameters of the 
#     tuning sequence
# ===========================================

# ---- Option #1 ---- 
# tuneGRid specifies the range of values to evalaute the model across
# Here I am specifying a wider range of max tree depths to evalaute my tree across
# NOTE: the column name for the dataframe must be spelled the same as the argument to the function
tg <- data.frame(maxdepth = c(2, 4, 6, 7, 8, 10, 15, 20, 25))

tune_results <-train(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                     data = data2,
                     method = "rpart2",
                     tuneGrid = tg)

# Plot the results of the tuning
plot(tune_results$results$maxdepth, tune_results$results$Accuracy)

# Print out the best parameter option
tune_results$bestTune

# ---- Option #2 ---- 
# trainControl specifies the computationa nuances of the resampling
# Here I am specifying 5-fold cross validation
tc <- trainControl(method = 'cv', number = 10)

tune_results <-train(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                     data = data2,
                     method = "rpart2",
                     tuneGrid = tg,
                     trControl = tc)

# Plot the results of the tuning
plot(tune_results$results$maxdepth, tune_results$results$Accuracy)

# ===========================================
#     Create a model with the 
#     best parameter value
# ===========================================
tuned_model <- rpart(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
                              data = train, 
                              method = "class",
                              control = rpart.control(maxdepth = tune_results$bestTune))

# Generate predictions (both probabilities and class predictions)
test$prediction <- predict(tuned_model, type = "prob", newdata = test)[,2] > 0.5

# Acccuracy in terms of classification rate (with 0.5 threshhold)
sum(test$prediction == test$survived) / nrow(test)