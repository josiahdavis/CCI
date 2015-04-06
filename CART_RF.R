#############################
# Basic Classification Tree
##############################

####
# Set up the workspace
####

rm(list=ls())     # clear the workspace
set.seed(973487)  # Ensures you can repeat the results
library(rpart)    # For creating the tree
library(partykit) # For plotting the tree
library(vcd)      # For mosaic plot

#####
# Get the data
####
load(url('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.sav'))
rand<-order(runif(dim(titanic3)[1]))  # Random indices
train <- titanic3[rand[1:655],]       # Training set
test <- titanic3[rand[656:1309],]     # Testing set

#####
# Create the tree
#####
form <- as.formula(as.factor(survived) ~ pclass + sex + age + sibsp) # Specify the variables of interest
tree <- rpart(form, train)          # Create the tree
plot(as.party(tree))                # Plot the tree
round(tree$variable.importance, 1)  # Check the importance (measured as avg. decrease in gini coefficient)

#####
# Evaluate the tree
#####
pred.train <- predict(tree, train); pred.test <- predict(tree, test)  # Generate the predictions for training and test set

# Mosaic plot for training data
pred.survival <- pred.train[,2]>0.5
actual.survival <- train$survived == 1
results <- table(actual.survival, pred.survival)
labels <- round(100*prop.table(results, 2), 1)
mosaic(results, pop = FALSE, main = "Tree Evaluated on Training Data")
labeling_cells(text = labels, margin = 0)(results)

# Mosaic plot for testing data
pred.survival.test <- pred.test[,2]>0.5
actual.survival.test <- test$survived == 1
results <- table(actual.survival.test, pred.survival.test)
labels <- round(100*prop.table(results, 2), 1)
mosaic(results, pop = FALSE, main = "Tree Evaluated on Test Data")
labeling_cells(text = labels, margin = 0)(results)


# Generate plain two way tables
round(prop.table(table(train$survived,pred.train[,2]>0.5), 1)*100, 2) # Correctly predicted from training set
round(prop.table(table(test$survived,pred.test[,2]>0.5), 1)*100, 2)   # Correctly predicted from test set


#####################
# Basic Random Forest
#####################
library(randomForest)
rf <- randomForest(form, train, importance = TRUE, na.action = na.omit)
sort(round(rf$importance[,4], 1), decreasing = TRUE) # Average decrease in the gini coefficient

pred.train.rf <- predict(rf, train); pred.test.rf <- predict(rf, test)
round(prop.table(table(train$survived,pred.train.rf), 1)*100, 2) # Correctly predicted from training set
round(prop.table(table(test$survived,pred.test.rf), 1)*100, 2)   # Correctly predicted from test set