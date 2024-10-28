#Packages to Install
#install.packages("e1071")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("reshape2")


# Read 'CustData2.csv' file into data frame 'customers'
customers <- read.csv("CustData2.csv")

# Display structure of the data frame
str(customers)

#DATA SELECTION
#Import libraries for plotting
library(ggplot2)
library(reshape2)

# Select numerical attributes
numeric_data <- customers[sapply(customers, is.numeric)]

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print(correlation_matrix)

melted_corr_matrix <- melt(correlation_matrix)
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Matrix", x = "Attributes", y = "Attributes")

# ** Cardinality **
# Create a function to calculate the cardinality (number of unique values)
calculate_cardinality <- function(df) {
  cardinalities <- sapply(df, function(x) length(unique(x)))
  return(cardinalities)
}

# Calculate the cardinality for each attribute in the dataset
cardinality <- calculate_cardinality(customers)

# Display the cardinality of each attribute
print("Cardinality (number of unique values) for each attribute:")
print(cardinality)

# Create a table or dataframe for better visualization
cardinality_df <- data.frame(Attribute = names(cardinality), Cardinality = cardinality)

#Sort the results by cardinality to easily identify attributes with high or low cardinality
cardinality_df <- cardinality_df[order(-cardinality_df$Cardinality),]

# Print the sorted cardinality dataframe
print(cardinality_df)

#DATA CLEANING
# Import 'lubridate' package to work with Date types
library(lubridate)

# Create a new column/attribute that calculates the customers age based on 'year of birth'
customers$Age <- as.integer(year(today()) - customers$year_of_birth)

# Display structure of the data frame
str(customers)

# Create vector with all columns/attributes that need to be kept
keepColumns <- c("Title", "Department.Name", "Annual.Salary", 
                 "Gross.Pay.Last.Paycheck", "Gross.Year.To.Date",
                 "Gross.Year.To.Date...FRS.Contribution",
                 "Age", "marital_status", "Country_id", "Education",
                 "Occupation", "household_size", "yrs_residence")

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

#Assign Customers to CustData
custData <- customers

#Rename Columns
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

#DATA AGGREGATION
library(e1071)
library(dplyr)

#Sum of Annual Salary by Department Name
Salary_By_Department <- custData %>%
  group_by(Department_Name) %>%
  summarise(Total_Annual_Salary = sum(Annual_Salary))

Salary_By_Department

#Average Annual Pay by Title
Average_Salary_By_Title <- custData %>%
  group_by(Title) %>%
  summarise(Average_Salary = mean(Annual_Salary))

Average_Salary_By_Title

#Customers by Eduaction Level
Customers_By_Education <- custData %>%
  group_by(Education) %>%
  summarise(Count = n())

Customers_By_Education

#Average Gross Year To Date by Age
Gross_Year_By_Age <- custData %>%
  group_by(Age) %>%
  summarise(Average_Gross_Year = mean(Gross_Year_To_Date))

Gross_Year_By_Age

# Average Household Size by Years of Residence
Household_Years_Residence <- custData %>%
  group_by(Years_Residence) %>%
  summarise(Average_Household_Size = mean(Household_Size))

Household_Years_Residence

#Average Annual Salary by Education
Salary_By_Education <- custData %>%
  group_by(Education) %>%
  summarise(Average_Salary_Education = mean(Annual_Salary))

Salary_By_Education

#Age by Occupation
Age_By_Occupation <- custData %>%
  group_by(Occupation) %>%
  summarise(Average_Age = mean(Age))

Age_By_Occupation 

# Number of Customers by Country
Employees_By_Country <- custData %>%
  group_by(Country_ID) %>%
  summarise(Count = n())

Employees_By_Country

#DATA TRANSFORMATION

#Categorisation
length(unique(custData$Title))
length(unique(custData$Department_Name))
length(unique(custData$Marital_Status))
length(unique(custData$Education))
length(unique(custData$Occupation))

#Categorise Marital Status
custData$Marital_Status <- as.factor(custData$Marital_Status)
table(custData$Marital_Status)

#Categorise Education
custData$Education <- as.factor(custData$Education)
table(custData$Education)

#Categorise Occupation
custData$Occupation <- as.factor(custData$Occupation)
table(custData$Occupation)

#Bin Salary
summary(custData$Annual_Salary)
custData$Salary_Group <- cut(custData$Annual_Salary, breaks = c(0, 42537, 58987, 83850, Inf),
                             labels = c("Low", "Medium", "High", "Very High"))
table(custData$Salary_Group)


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

# Export to CSV file
write.csv(custData, "CustData2_Prepared.csv", row.names = FALSE)
