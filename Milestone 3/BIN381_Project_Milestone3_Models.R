#Load Libraries
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(caTools)
library(randomForest)

#Load the dataset
custData <- read.csv("CustData2_Prepared.csv")

#Explore the structure of the dataset
str(custData)

#Split the dataset to 80% training data and 20% testing data
set.seed(123)
train_index <- createDataPartition(custData$Eligible, p = 0.8, list = FALSE)
train_data <- custData[train_index, ]
test_data <- custData[-train_index, ]

##LOGISTIC REGRESSION
#Build Logistic Regression Model
logisticRegressionModel <- glm(formula = Eligible~ . -Annual_Salary,
                               data = train_data,family = 'binomial')
summary(logisticRegressionModel)

#Make Predictions using Logistic Regression
logisticRegressionPrediction <- predict(logisticRegressionModel, newdata = test_data, type ='response')
head(logisticRegressionPrediction)
logisticRegressionY_pred = ifelse(logisticRegressionPrediction >0.5, 1, 0)

#Confusion matrix of Logistic Regression
logisticRegression_matrix <- table(actual = test_data$Eligible, predicted = logisticRegressionY_pred)
logisticRegression_matrix

logisticRegression_truePositive <- logisticRegression_matrix[1, 1]
logisticRegression_trueNegative <- logisticRegression_matrix[2, 2]
logisticRegression_falsePositive <- logisticRegression_matrix[1, 2]
logisticRegression_falseNegative <- logisticRegression_matrix[2, 1]

# Calculate Evaluation Metrics
logisticRegression_accuracy <- round((sum(diag(logisticRegression_matrix)) / sum(logisticRegression_matrix)), 2)
logisticRegression_precision <- round(logisticRegression_truePositive / (logisticRegression_truePositive + logisticRegression_falsePositive), 2)
logisticRegression_recall <- round(logisticRegression_truePositive / (logisticRegression_truePositive + logisticRegression_falseNegative), 2)
logisticRegression_f1_score <- round(2 * (logisticRegression_precision * logisticRegression_recall) / (logisticRegression_precision + logisticRegression_recall), 2)

##DECISION TREE
#Build Decision Tree Model
decisionTreeModel <- rpart(Eligible ~ . -Annual_Salary, data = train_data, method = 'class')

# Visualize the model
rpart.plot(decisionTreeModel, type = 3, extra = 101, under = TRUE, fallen.leaves = TRUE)

# Make predictions on the test data
decisionTreePredictions <- predict(decisionTreeModel, newdata = test_data, type = 'class')

#Confusion matrix
decisionTreeMatrix <- table(test_data$Eligible, decisionTreePredictions)
print(decisionTreeMatrix)

decisionTreeTruePositive <- decisionTreeMatrix[1, 1]
decisionTreeTrueNegative <- decisionTreeMatrix[2, 2]
decisionTreeFalsePositive <- decisionTreeMatrix[1, 2]
decisionTreeFalseNegative <- decisionTreeMatrix[2, 1]

#Calculate Evaluation Metrics
decisionTreeAccuracy <- round((sum(diag(decisionTreeMatrix)) / sum(decisionTreeMatrix)), 2)
decisionTreePrecision <- round(decisionTreeTruePositive / (decisionTreeTruePositive + decisionTreeFalsePositive), 2)
decisionTreeRecall <- round(decisionTreeTruePositive / (decisionTreeTruePositive + decisionTreeFalseNegative), 2)
decisionTreeF1Score <- round(2 * (decisionTreePrecision * decisionTreeRecall) / (decisionTreePrecision + decisionTreeRecall), 2)

##RANDOM FOREST
#Reload the dataset
custData <- read.csv("CustData2_Prepared.csv")
custData$Eligible <- as.factor(custData$Eligible)

#Split the dataset to 80% training data and 20% testing data
set.seed(123)
train_index <- createDataPartition(custData$Eligible, p = 0.8, list = FALSE)
train_data <- custData[train_index, ]
test_data <- custData[-train_index, ]

#Build Random Forest Model
randomForest_model <- randomForest(Eligible ~ . -Annual_Salary, data = train_data, ntree = 100, mtry = 3, importance = TRUE)

#The attribute importance can be visualised using the random forest model
varImpPlot(randomForest_model)

#Make Predictions Using Random Forest
randomForest_predictions <- predict(randomForest_model, newdata = test_data)

#Matrix for Random Forest
randomForest_cm <- confusionMatrix(as.factor(randomForest_predictions), as.factor(test_data$Eligible))
randomForest_matrix <- randomForest_cm$table

randomForest_truePositive <- randomForest_matrix[1, 1]
randomForest_trueNegative <- randomForest_matrix[2, 2]
randomForest_falsePositive <- randomForest_matrix[1, 2]
randomForest_falseNegative <- randomForest_matrix[2, 1]

#Calculate Evaluation Metrics
randomForest_accuracy <- round((sum(diag(randomForest_matrix)) / sum(randomForest_matrix)), 2)
randomForest_precision <- round(randomForest_truePositive / (randomForest_truePositive + randomForest_falsePositive), 2)
randomForest_recall <- round(randomForest_truePositive / (randomForest_truePositive + randomForest_falseNegative), 2)
randomForest_f1_score <- round(2 * (randomForest_precision * randomForest_recall) / (randomForest_precision + randomForest_recall), 2)

##Print Evaluation Metrics of All Models
cat("Logistic Regression Accuracy:", logisticRegression_accuracy, "\n")
cat("Logistic Regression Precision:", logisticRegression_precision, "\n")
cat("Logistic Regression Recall:", logisticRegression_recall, "\n")
cat("Logistic Regression F1-score:", logisticRegression_f1_score, "\n")

cat("Decision Tree Accuracy:", decisionTreeAccuracy, "\n")
cat("Decision Tree Precision:", decisionTreePrecision, "\n")
cat("Decision Tree Recall:", decisionTreeRecall, "\n")
cat("Decision Tree F1-score:", decisionTreeF1Score, "\n")

cat("Random Forest Accuracy:", randomForest_accuracy, "\n")
cat("Random Forest Precision:", randomForest_precision, "\n")
cat("Random Forest Recall:", randomForest_recall, "\n")
cat("Random Forest F1-score:", randomForest_f1_score, "\n")
