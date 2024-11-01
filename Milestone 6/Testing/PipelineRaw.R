# Load necessary libraries
library(lubridate)
library(dplyr)

# Load the dataset and calculate frequency tables
custData <- read.csv("CustData2.csv")

# Calculate frequency tables with column names aligned to match new_record
title_frequency <- table(custData$Title)
department_frequency <- table(custData$Department.Name)
country_frequency <- table(custData$Country_id)

# Define preprocessing function for a single record
preprocess_record <- function(record, title_freq, dept_freq, country_freq) {
  # Feature Engineering
  record$Age <- as.integer(year(today()) - record$year_of_birth)
  record$Eligible <- ifelse(record$Annual.Salary > 50000, 1, 0)
  
  # Clean marital status values
  record$marital_status <- tolower(record$marital_status)
  if (record$marital_status %in% c("married", "mar-af")) {
    record$marital_status <- "married"
  } else if (record$marital_status %in% c("neverm", "mabsent")) {
    record$marital_status <- "single"
  } else if (record$marital_status %in% c("divorc.", "separ.")) {
    record$marital_status <- "divorced"
  } else if (record$marital_status %in% c("widow", "widowed")) {
    record$marital_status <- "widowed"
  }
  
  # Fill missing marital status with mode (e.g., "married")
  if (is.na(record$marital_status) || record$marital_status == "") {
    record$marital_status <- "married"  # Assuming "married" is the mode
  }
  
  # Outlier Capping (1st and 99th percentiles)
  cap_outliers <- function(value, lower_cap, upper_cap) {
    if (value < lower_cap) return(lower_cap)
    if (value > upper_cap) return(upper_cap)
    return(value)
  }
  
  record$Annual.Salary <- cap_outliers(record$Annual.Salary, 42537, 83850)
  record$Gross.Pay.Last.Paycheck <- cap_outliers(record$Gross.Pay.Last.Paycheck, 0, 100000)
  record$Gross.Year.To.Date <- cap_outliers(record$Gross.Year.To.Date, 0, 300000)
  record$Gross.Year.To.Date...FRS.Contribution <- cap_outliers(record$Gross.Year.To.Date...FRS.Contribution, 0, 50000)
  
  # Encoding (frequency encoding using pre-calculated tables)
  record$Frequency_Title <- ifelse(record$Title %in% names(title_freq), title_freq[record$Title], 0)
  record$Frequency_Department <- ifelse(record$Department.Name %in% names(dept_freq), dept_freq[record$Department.Name], 0)
  record$Frequency_Country_ID <- ifelse(record$Country_id %in% names(country_freq), country_freq[record$Country_id], 0)
  
  # One-hot encode marital status
  record$Marital_Status_married <- ifelse(record$marital_status == "married", 1, 0)
  record$Marital_Status_single <- ifelse(record$marital_status == "single", 1, 0)
  record$Marital_Status_divorced <- ifelse(record$marital_status == "divorced", 1, 0)
  record$Marital_Status_widowed <- ifelse(record$marital_status == "widowed", 1, 0)
  
  record$Education_Bach <- ifelse(record$Education == "Bach.", 1, 0)
  record$Education_Masters <- ifelse(record$Education == "Masters", 1, 0)
  record$Education_HS <- ifelse(record$Education == "HS-grad", 1, 0)
  
  record$Occupation_Cleric <- ifelse(record$Occupation == "Cleric.", 1, 0)
  record$Occupation_Prof <- ifelse(record$Occupation == "Prof.", 1, 0)
  record$Occupation_Exec <- ifelse(record$Occupation == "Executive", 1, 0)
  record$Occupation_Sales <- ifelse(record$Occupation == "Sales", 1, 0)
  
  record <- record %>% select(-year_of_birth,-marital_status, -Education, -Occupation)
  
  # Return preprocessed record
  return(record)
}

# Test the function with a single record
new_record <- data.frame(
  Title = "CORRECTIONAL OFFICER",
  Department.Name = "CORRECTIONS & REHABILITATION",
  Annual.Salary = 55000,
  Gross.Pay.Last.Paycheck = 2000,
  Gross.Year.To.Date = 30000,
  Gross.Year.To.Date...FRS.Contribution = 1500,
  year_of_birth = 1985,
  marital_status = "single",
  Country_id = "52770",
  Education = "Bach.",
  Occupation = "Sales",
  household_size = 3,
  yrs_residence = 5
)

preprocessed_record <- preprocess_record(new_record, title_frequency, department_frequency, country_frequency)
print(preprocessed_record)
