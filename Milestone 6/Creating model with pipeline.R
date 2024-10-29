# Load necessary libraries
library(dplyr)
library(lubridate)
library(caret)
library(randomForest)

# Define the Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Read 'CustData2.csv' file into data frame 'customers'
customers <- read.csv("CustData2.csv")

# Define preprocessing pipeline function
create_preprocessing_pipeline <- function(data) {
  # Step 1: Feature Engineering
  data$Age <- as.integer(year(today()) - data$year_of_birth)
  data$Eligible <- ifelse(data$Annual.Salary > 50000, 1, 0)
  
  # Remove unnecessary columns
  keepColumns <- c("Title", "Department.Name", "Annual.Salary", 
                   "Gross.Pay.Last.Paycheck", "Gross.Year.To.Date",
                   "Gross.Year.To.Date...FRS.Contribution",
                   "Age", "marital_status", "Country_id", "Education",
                   "Occupation", "household_size", "yrs_residence", "Eligible")
  data <- data[keepColumns]
  
  # Step 2: Data Cleaning
  data$marital_status <- tolower(data$marital_status)
  data$marital_status <- recode(data$marital_status, "married" = "married", 
                                "mar-af" = "married", "neverm" = "single",
                                "mabsent" = "single", "divorc." = "divorced",
                                "separ." = "divorced", "widow" = "widowed",
                                "widowed" = "widowed")

  # Fill missing values for marital status with mode
  data$marital_status[is.na(data$marital_status) | data$marital_status == ""] <- 
    Mode(data$marital_status)
  
  # Remove empty cells for all columns/attributes
  customers <- customers[!(is.na(customers$Title) | customers$Title == "" |
                             is.na(customers$Department.Name)  |
                             customers$Department.Name == ""  |
                             is.na(customers$Annual.Salary)  |
                             customers$Annual.Salary == ""  |
                             is.na(customers$Gross.Pay.Last.Paycheck)  |
                             customers$Gross.Pay.Last.Paycheck == ""  |
                             is.na(customers$Gross.Year.To.Date)  |
                             customers$Gross.Year.To.Date == ""  |
                             is.na(customers$Gross.Year.To.Date...FRS.Contribution)  |
                             customers$Gross.Year.To.Date...FRS.Contribution == ""), ]
  
  # Step 3: Outlier Treatment
  cap_outliers <- function(column) {
    lower_cap <- quantile(column, 0.01, na.rm = TRUE)
    upper_cap <- quantile(column, 0.99, na.rm = TRUE)
    column[column < lower_cap] <- lower_cap
    column[column > upper_cap] <- upper_cap
    return(column)
  }
  num_vars <- c("Annual.Salary", "Gross.Pay.Last.Paycheck", 
                "Gross.Year.To.Date", "Gross.Year.To.Date...FRS.Contribution")
  data[num_vars] <- lapply(data[num_vars], cap_outliers)
  
  # Step 4: Encoding and Scaling
  # Convert categorical variables
  data$Marital_Status <- factor(data$marital_status)
  data$Education <- factor(data$Education)
  data$Occupation <- factor(data$Occupation)
  
  # Create preprocessing pipeline
  preprocess_pipeline <- preProcess(data, method = c("center", "scale", "BoxCox"))
  
  return(list(data = data, pipeline = preprocess_pipeline))
}

# Apply preprocessing pipeline
processed_data <- create_preprocessing_pipeline(customers)
custData <- predict(processed_data$pipeline, newdata = processed_data$data)

# Ensure there are no missing values in the target variable 'Eligible'
custData <- custData[!is.na(custData$Eligible), ]

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(custData$Eligible, p = 0.8, list = FALSE)
train_data <- custData[train_index, ]
test_data <- custData[-train_index, ]

# Train Random Forest Model using pipeline
train_data$Eligible <- as.factor(train_data$Eligible)
randomForest_model <- randomForest(Eligible ~ ., data = train_data, ntree = 100, 
                                   mtry = 3, importance = TRUE)

# Predictions using the pipeline for new data
test_data_processed <- predict(processed_data$pipeline, newdata = test_data)
randomForest_predictions <- predict(randomForest_model, newdata = test_data_processed)

# Ensure both prediction and actual values have the same levels
randomForest_predictions <- as.factor(randomForest_predictions)
test_data_processed$Eligible <- as.factor(test_data_processed$Eligible)

# Check and adjust levels if necessary
levels(randomForest_predictions) <- levels(test_data_processed$Eligible)

# Confusion Matrix for Random Forest
confusionMatrix(randomForest_predictions, as.factor(test_data_processed$Eligible))

# Save the model and preprocessing pipeline
saveRDS(randomForest_model, file = "random_forest_model.rds")
saveRDS(processed_data$pipeline, file = "preprocessing_pipeline.rds")
