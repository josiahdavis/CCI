#############################################
#
#     LOGISTIC  &  PROBIT  REGRESSION
#
#############################################


# ===========================================
#     Set up the workspace and get the data
# ===========================================

rm(list=ls()); gc()     # clear the workspace
set.seed(973487)        # Ensures you can repeat the results
setwd("C:/Users/josdavis/Documents/Personal/GitHub/CCI")

# Get the data
data <- read.csv("titanic.csv", header = TRUE)

# Convert survived to True/False
data$survived = data$survived == 'survived'

# Remove any records with missing values
data = na.omit(data)

# Split into training and testing sets
idxs <- runif(nrow(data)) < 0.7   # Random Indices
train <- data[idxs, ]             # Training set
test  <- data[!idxs, ]            # Testing set
rm(idxs, data)
summary(train)

# ===========================================
#       Run the regression
# ===========================================

lr <- glm(survived ~ pclass + sex + age + sibsp + parch,
          family = binomial(link = logit),
          data = train)

# View the model summary
summary(lr)

# View the table with coefficients only 
summary(lr)$coefficients

# View the coefficients and p-values only
summary(lr)$coefficients[,c(1, 4)]

# Running a probit regression is as simple as changing the link function
pr <- glm(survived ~ pclass + sex + age + sibsp + parch, 
          family = binomial(link = probit), 
          data = train)

summary(pr)


# ===========================================
#       Modify the specification
# ===========================================

# Add an interaction term
lr <- glm(survived ~ pclass + sex + age + pclass:age + sibsp + parch, 
          family = binomial(link = logit), 
          data = train)

# ===========================================
#       Evaluate the accuracy of the regressions
# ===========================================

# Generate predictions (both probabilities and class predictions)
test$predict_proba <- predict(lr, test)
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