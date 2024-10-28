# Read 'CustData2.csv' file into data frame 'customers'
customers <- read.csv("CustData2.csv")

#DATA CLEANING
# Import 'lubridate' package to work with Date types
library(lubridate)

# Create a new column/attribute that calculates the customers age based on 'year of birth'
customers$Age <- as.integer(year(today()) - customers$year_of_birth)

# Create the target attribute that states whether the person is eligible or not
customers$Eligible <- ifelse(customers$Annual.Salary > 50000, 1, 0)

# Display structure of the data frame
str(customers)

# Create vector with all columns/attributes that need to be kept
keepColumns <- c("Title", "Department.Name", "Annual.Salary", 
                 "Gross.Pay.Last.Paycheck", "Gross.Year.To.Date",
                 "Gross.Year.To.Date...FRS.Contribution",
                 "Age", "marital_status", "Country_id", "Education",
                 "Occupation", "household_size", "yrs_residence", "Eligible")

# Remove irrelevant columns/attributes by keeping relevant ones
customers <- customers[keepColumns]

# Display structure of the data frame
str(customers)


# Cleaning "marital_status"
# Display all of the unique values contained in the 'marital_status' column/attribute
unique(customers$marital_status)

# Count the unique values contained in the 'marital_status' column/attribute
length(unique(customers$marital_status))

# Replace incorrect values for "marital_status"
for (i in 1:nrow(customers)) {
  if (customers$marital_status[i] == "Married") {
    customers$marital_status[i] <- "married"
  } else if (customers$marital_status[i] == "Mar-AF") {
    customers$marital_status[i] <- "married"
  } else if (customers$marital_status[i] == "NeverM") {
    customers$marital_status[i] <- "single"
  } else if (customers$marital_status[i] == "Mabsent") {
    customers$marital_status[i] <- "single"
  } else if (customers$marital_status[i] == "Divorc.") {
    customers$marital_status[i] <- "divorced"
  } else if (customers$marital_status[i] == "Separ.") {
    customers$marital_status[i] <- "divorced"
  } else if (customers$marital_status[i] == "widow") {
    customers$marital_status[i] <- "widowed"
  } else if (customers$marital_status[i] == "Widowed") {
    customers$marital_status[i] <- "widowed"
  }
}

# Check to see if "marital_status" was cleaned successfully
unique(customers$marital_status)
length(unique(customers$marital_status))

# Populating "marital_status"
# Count the number of empty cells
sum(customers$marital_status=="")

# Function to calculate mode
get_mode <- function(v) {
  uniq_vals <- unique(v)
  uniq_vals[which.max(tabulate(match(v, uniq_vals)))]
}

# Get mode value from function
mode_value <- get_mode(customers$marital_status[!is.na(customers$marital_status) &
                                                  customers$marital_status != ""])

# Fill missing or empty values in "marital_status" column with mode
customers$marital_status[is.na(customers$marital_status) |
                           customers$marital_status == ""] <- mode_value

# Check if "marital_status" is filled
sum(customers$marital_status=="")


# Missing Values
sum(customers$Title=="")
sum(customers$Department.Name=="")
sum(is.na(customers$Annual.Salary))
sum(is.na(customers$Gross.Pay.Last.Paycheck))
sum(is.na(customers$Gross.Year.To.Date))
sum(is.na(customers$Gross.Year.To.Date...FRS.Contribution))
sum(is.na(customers$Age))
sum(customers$marital_status=="")
sum(is.na(customers$Country_id))
sum(customers$Education=="")
sum(customers$Occupation=="")
sum(is.na(customers$household_size))
sum(is.na(customers$yrs_residence))

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

# Check if there are empty cells left
sum(customers$Title=="")
sum(customers$Department.Name=="")
sum(is.na(customers$Annual.Salary))
sum(is.na(customers$Gross.Pay.Last.Paycheck))
sum(is.na(customers$Gross.Year.To.Date))
sum(is.na(customers$Gross.Year.To.Date...FRS.Contribution))
sum(is.na(customers$Age))
sum(is.na(customers$Country_id))
sum(customers$Education=="")
sum(customers$Occupation=="")
sum(is.na(customers$household_size))
sum(is.na(customers$yrs_residence))

# ** Outlier Treatment **
## Display outliers
### Display "Annual.Salary" box plot
boxplot(customers$Annual.Salary,
        main = "Annual Salary Box Plot",
        ylab = "Annual.Salary")
### Display "Gross.Pay.Last.Paycheck" box plot
boxplot(customers$Gross.Pay.Last.Paycheck,
        main = "Gross Pay Last Paycheck Box Plot",
        ylab = "Gross.Pay.Last.Paycheck")
### Display "Gross.Year.To.Date" box plot
boxplot(customers$Gross.Year.To.Date,
        main = "Gross Year To Date Box Plot",
        ylab = "Gross.Year.To.Date")
### Display "Gross.Year.To.Date...FRS.Contribution" box plot
boxplot(customers$Gross.Year.To.Date...FRS.Contribution,
        main = "Gross Year To Date ... FRS Contribution Box Plot",
        ylab = "Gross.Year.To.Date...FRS.Contribution")

# Capping outliers using the 1st and 99th percentiles
cap_outliers <- function(column) {
  lower_cap <- quantile(column, 0.01)
  upper_cap <- quantile(column, 0.99)
  column[column < lower_cap] <- lower_cap
  column[column > upper_cap] <- upper_cap
  return(column)
}

# Apply capping to the numeric columns
customers$Annual.Salary <- cap_outliers(customers$Annual.Salary)
customers$Gross.Pay.Last.Paycheck <- cap_outliers(customers$Gross.Pay.Last.Paycheck)
customers$Gross.Year.To.Date <- cap_outliers(customers$Gross.Year.To.Date)
customers$Gross.Year.To.Date...FRS.Contribution <- cap_outliers(customers$Gross.Year.To.Date...FRS.Contribution)

# Check if outliers are fixed
## Display "Annual.Salary" box plot
boxplot(customers$Annual.Salary,
        main = "Annual Salary Box Plot",
        ylab = "Annual.Salary")
## Display "Gross.Pay.Last.Paycheck" box plot
boxplot(customers$Gross.Pay.Last.Paycheck,
        main = "Gross Pay Last Paycheck Box Plot",
        ylab = "Gross.Pay.Last.Paycheck")
## Display "Gross.Year.To.Date" box plot
boxplot(customers$Gross.Year.To.Date,
        main = "Gross Year To Date Box Plot",
        ylab = "Gross.Year.To.Date")
## Display "Gross.Year.To.Date...FRS.Contribution" box plot
boxplot(customers$Gross.Year.To.Date...FRS.Contribution,
        main = "Gross Year To Date ... FRS Contribution Box Plot",
        ylab = "Gross.Year.To.Date...FRS.Contribution")

# Check the numerical values
summary(customers)

# Preprocessing
# Assign Customers to CustData
custData <- customers

# Rename Columns
names(custData)[2] <- 'Department_Name'
names(custData)[3] <- 'Annual_Salary'
names(custData)[4] <- 'Gross_Pay_Last_Paycheck'
names(custData)[5] <- 'Gross_Year_To_Date'
names(custData)[6] <- 'Gross_Year_To_Date_FRS_Contribution'
names(custData)[8] <- 'Marital_Status'
names(custData)[9] <- 'Country_ID'
names(custData)[12] <- 'Household_Size'
names(custData)[13] <- 'Years_Residence'
names(custData)

#DATA TRANSFORMATION

# Factorization
#Categorisation
length(unique(custData$Title))
length(unique(custData$Department_Name))
length(unique(custData$Marital_Status))
length(unique(custData$Country_ID))
length(unique(custData$Education))
length(unique(custData$Occupation))

# Convert categorical variables to factors
custData$Marital_Status <- as.factor(custData$Marital_Status)
custData$Education <- as.factor(custData$Education)
custData$Occupation <- as.factor(custData$Occupation)
str(custData)

# Bin Salary
summary(custData$Annual_Salary)
custData$Salary_Group <- cut(custData$Annual_Salary, breaks = c(0, 42537, 58987, 83850, Inf),
                             labels = c("Low", "Medium", "High", "Very High"))
table(custData$Salary_Group)

# One hot encoding of categorical data
custData <- cbind(custData, model.matrix(~Marital_Status - 1, data = custData))
custData <- cbind(custData, model.matrix(~Education - 1, data = custData))
custData <- cbind(custData, model.matrix(~Occupation - 1, data = custData))
custData <- cbind(custData, model.matrix(~Salary_Group - 1, data = custData))

str(custData)

#Frequency Encoding:
#Title Encoding:
Title_Frequency <- table(custData$Title)
Title_Frequency_DF <- data.frame(Title = names(Title_Frequency), Frequency_Title = as.vector(Title_Frequency))
custData <- merge(custData, Title_Frequency_DF, by = "Title")
head(custData$Frequency_Title)

#Department Encoding:
Department_Frequency <- table(custData$Department_Name)
Department_Frequency_DF <- data.frame(Department_Name = names(Department_Frequency), Frequency_Department = as.vector(Department_Frequency))
custData <- merge(custData, Department_Frequency_DF, by = "Department_Name")
head(custData$Frequency_Department)

#Country_ID Encoding:
Country_ID_Frequency <- table(custData$Country_ID)
Country_ID_Frequency_DF <- data.frame(Country_ID = names(Country_ID_Frequency), Frequency_Country_ID = as.vector(Country_ID_Frequency))
custData <- merge(custData, Country_ID_Frequency_DF, by = "Country_ID")
head(custData$Frequency_Country_ID)

names(custData)

#Standardisation/Normalisation
library(e1071)
skewness_Annual_Salary <- skewness(custData$Annual_Salary)
print(skewness_Annual_Salary)

skewness_Gross_Pay_Last_Paycheck <- skewness(custData$Gross_Pay_Last_Paycheck)
print(skewness_Gross_Pay_Last_Paycheck)

skewness_Gross_Year_To_Date <- skewness(custData$Gross_Year_To_Date)
print(skewness_Gross_Year_To_Date)

skewness_Gross_Year_To_Date_FRS_Contribution <- skewness(custData$Gross_Year_To_Date_FRS_Contribution)
print(skewness_Gross_Year_To_Date_FRS_Contribution)

skewness_Age <- skewness(custData$Age)
print(skewness_Age)

#Robust Scaling

library(dplyr)
robustScaling <- function(x)
{
  median <- median(x)
  iqr <- IQR(x)
  return((x-median)/iqr)
}

custData <- custData %>%
  mutate(Annual_Salary = robustScaling(Annual_Salary))

custData <- custData %>%
  mutate(Gross_Pay_Last_Paycheck = robustScaling(Gross_Pay_Last_Paycheck))

custData <- custData %>%
  mutate(Gross_Year_To_Date = robustScaling(Gross_Year_To_Date))

custData <- custData %>%
  mutate(Gross_Year_To_Date_FRS_Contribution = robustScaling(Gross_Year_To_Date_FRS_Contribution))

#Z-Score Normalisation
custData <- custData %>%
  mutate(Age = (Age - mean(Age)) / sd(Age))

#Observe how the dataset has been transformed
head(custData)
str(custData)
names(custData)

# Create vector with all columns/attributes that need to be kept
keepColumns <- c("Annual_Salary", "Gross_Pay_Last_Paycheck", "Gross_Year_To_Date",                 
                 "Gross_Year_To_Date_FRS_Contribution", "Age", "Household_Size",                     
                 "Years_Residence", "Marital_Statusdivorced", "Marital_Statusmarried",
                 "Marital_Statussingle", "Marital_Statuswidowed", "EducationBach.",
                 "EducationHS-grad", "EducationMasters", "OccupationCleric.",
                 "OccupationExec.", "OccupationProf.", "OccupationSales", 
                 "Salary_GroupLow", "Salary_GroupMedium", "Salary_GroupHigh", 
                 "Salary_GroupVery High", "Frequency_Title", "Frequency_Department", 
                 "Frequency_Country_ID", "Eligible")

# Remove irrelevant columns/attributes by keeping relevant ones
custData <- custData[keepColumns]

str(custData)

#Data Balancing
table(custData$Eligible)
#Plot target attribute to view imbalance
barplot(table(custData$Eligible))

library(caret)
library(ggplot2)

#Find the number of customers who are eligible and the number of those not eligible
majority_count <- sum(custData$Eligible == 1)
minority_count <- sum(custData$Eligible == 0)

#Find the imbalance Ratio
imbalance_ratio <- majority_count / minority_count
cat("Imbalance ratio:", imbalance_ratio, "\n")

# Export to CSV file
write.csv(custData, "CustData2_Prepared.csv", row.names = FALSE)
