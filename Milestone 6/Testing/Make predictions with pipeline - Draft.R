# Load necessary libraries
library(dplyr)
library(caret)
library(lubridate)  # For date functions

# Define the Mode function (in case you use it in the pipeline)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Load the saved preprocessing pipeline and random forest model
preprocessing_pipeline <- readRDS("preprocessing_pipeline.rds")
randomForest_model <- readRDS("random_forest_model.rds")

# Example new user data in data frame format
new_data <- data.frame(
  Column1 = 1, Last.Name = "ALBERT", First.Name = "JESSICA", Middle.Initial = "M",
  Title = "CORRECTIONAL OFFICER", Department.Name = "CORRECTIONS & REHABILITATION",
  Annual.Salary = 54620, Gross.Pay.Last.Paycheck = 2502, Gross.Year.To.Date = 48025,
  Gross.Year.To.Date...FRS.Contribution = 46617, year_of_birth = 1976, 
  marital_status = "married", street_address = "27 North Sagadahoc Boulevard",
  postal_code = 60332, city = "Ede", State = "Gelderland", Province = "", 
  Country_id = 52770, phone_number = "519-236-6123", email = "Ruddy@company.com", 
  Education = "Masters", Occupation = "Prof.", household_size = 2, yrs_residence = 4
)

# Step 1: Feature Engineering
new_data <- new_data %>%
  mutate(
    Age = as.integer(year(today()) - year_of_birth),  # Calculate Age
    Eligible = ifelse(Annual.Salary > 50000, 1, 0)    # Dummy Eligible column (not used for prediction)
  )

# Step 2: Select relevant columns based on the pipeline
# Retrieve column names expected by the pipeline (from training data)
expected_columns <- names(preprocessing_pipeline$mean)

# Ensure the new data has the same columns as expected by the pipeline
new_data <- new_data %>%
  select(all_of(expected_columns))

# Step 3: Apply the preprocessing pipeline to the new data
processed_new_data <- predict(preprocessing_pipeline, newdata = new_data)

# Step 4: Make predictions using the random forest model
predictions <- predict(randomForest_model, newdata = processed_new_data)

# Display prediction results
cat("Prediction for Eligibility (1 = Eligible, 0 = Not Eligible):", predictions, "\n")
