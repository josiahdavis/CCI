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
#       Run the regression
# ===========================================

lr <- glm(survived ~ pclass + sex + age + sibsp + parch, 
             family = binomial(logit), 
             data = train)

pr <- glm(survived ~ pclass + sex + age + sibsp + parch, 
          family = binomial(probit), 
          data = train)

summary(lr)


# ===========================================
#       Modify the specification
# ===========================================

# Add an interaction term
lr <- glm(survived ~ pclass + sex + age + pclass:age + sibsp + parch, 
          family = binomial(logit), 
          data = train)

