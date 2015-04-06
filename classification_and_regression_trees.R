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
summary(train)

#####
# Create the tree
#####
tree <- rpart(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
              data = train, 
              method = "class")

# View the tree
tree

# View the details of each node
summary(tree)

# View the importance scores (avg. decrease in gini coefficient)
tree$variable.importance

#####
# Control the parameters of the tree
#####

# The control argument allows you to limit how large the tree grows
# For example: minsplit = 30 stops splitting once a node has 30 or less data points
tree <- rpart(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
              data = train,
              method = "class",
              control = rpart.control(minsplit = 30))

# Another example: maxdepth = 4 limits the depth of the tree to 4 levels (inlcuding terminal node)
tree <- rpart(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
              data = train,
              method = "class",
              control = rpart.control(maxdepth = 4))

# See the documentation for default values and more options
?rpart.control

#################
# Missing Values
#################

# Remove records with missing response or ALL missing inputs (DEFALUT)
tree <- rpart(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
              data = train,
              method = "class",
              na.action = na.rpart)

# Missing values (remove rows with any missing values)
tree <- rpart(as.factor(survived) ~ pclass + sex + age + sibsp + parch, 
              data = train,
              method = "class",
              na.action = na.omit)

#####
# Evaluate the tree
#####
predictions <- predict(tree, test)  # Generate the predictions for training and test set

# Mosaic plot for testing data
pred.survival.test <- pred.test[,2]>0.5
actual.survival.test <- test$survived == 1
results <- table(actual.survival.test, pred.survival.test)
labels <- round(100*prop.table(results, 2), 1)
mosaic(results, pop = FALSE, main = "Tree Evaluated on Test Data")
labeling_cells(text = labels, margin = 0)(results)

# Generate plain two way tables
round(prop.table(table(test$survived,pred.test[,2]>0.5), 1)*100, 2)   # Correctly predicted from test set

########
# Plot the tree
########
plot(as.party(tree))
