# ============================
#     Classification Tree
# ============================

####
# Set up the workspace
####
rm(list=ls()); gc()     # clear the workspace
set.seed(973487)        # Ensures you can repeat the results
library(rpart)          # For creating the tree
library(partykit)       # For plotting the tree
setwd("C:/Users/josdavis/Documents/Personal/GitHub/CCI")

#####
# Get the data
####
data <- read.csv("titanic.csv", header = TRUE)
idxs <- runif(nrow(data)) < 0.7   # Random Indices
train <- data[idxs, ]             # Training set
test  <- data[!idxs, ]            # Testing set

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
pred.test <- predict(tree, test)  # Generate the predictions for training and test set

# Mosaic plot for testing data
pred.survival.test <- pred.test[,2]>0.5
actual.survival.test <- test$survived == 1
results <- table(actual.survival.test, pred.survival.test)
labels <- round(100*prop.table(results, 2), 1)
mosaic(results, pop = FALSE, main = "Tree Evaluated on Test Data")
labeling_cells(text = labels, margin = 0)(results)

# Generate plain two way tables
round(prop.table(table(test$survived,pred.test[,2]>0.5), 1)*100, 2)   # Correctly predicted from test set

# ============================
#     Random Forest
# ============================
library(randomForest)
rf <- randomForest(form, train, importance = TRUE, na.action = na.omit)
sort(round(rf$importance[,4], 1), decreasing = TRUE) # Average decrease in the gini coefficient

pred.test.rf <- predict(rf, test)
round(prop.table(table(test$survived,pred.test.rf), 1)*100, 2)   # Correctly predicted from test set