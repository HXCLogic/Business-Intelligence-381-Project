# Set the working directory
setwd("C:/Users/hroux/OneDrive - belgiumcampus.ac.za/Documents/Year 3/#9 - BIN381/Project/Milestone 1")


# Read the dataset into the dataframe "customers"
customers <- read.csv("CustData2.csv")


# Missing Values
sum(is.na(customers$Column1))
sum(customers$Last.Name=="")
sum(customers$First.Name=="")
sum(customers$Middle.Initial=="")
sum(customers$Title=="")
sum(customers$Department.Name=="")
sum(is.na(customers$Annual.Salary))
sum(is.na(customers$Gross.Pay.Last.Paycheck))
sum(is.na(customers$Gross.Year.To.Date))
sum(is.na(customers$Gross.Year.To.Date...FRS.Contribution))
sum(is.na(customers$year_of_birth))
sum(customers$marital_status=="")
sum(customers$street_address=="")
sum(is.na(customers$postal_code))
sum(customers$city=="")
sum(customers$State=="")
sum(customers$Province=="")
sum(is.na(customers$Country_id))
sum(customers$phone_number=="")
sum(customers$email=="")
sum(customers$Education=="")
sum(customers$Occupation=="")
sum(is.na(customers$household_size))
sum(is.na(customers$yrs_residence))


# Identify duplicate rows / records and insert into the dataframe "duplicated_rows"
duplicated_rows <- customers[duplicated(customers), ]
# Display "duplicated_rows" and all its records
print(duplicated_rows)
# Count the number of duplicate rows
sum(duplicated_rows)


# Outliers shown in box plot of the "Annual.Salary"
boxplot(customers$Annual.Salary)


# Variable Type Validation
str(customers)


# Data Distribution
hist(customers$Annual.Salary)


# Unique records within attributes that contain "characters"
length(unique(customers$Last.Name))
length(unique(customers$First.Name))
length(unique(customers$Middle.Initial))
length(unique(customers$Title))
length(unique(customers$Department.Name))
length(unique(customers$marital_status))
length(unique(customers$street_address))
length(unique(customers$city))
length(unique(customers$State))
length(unique(customers$Province))
length(unique(customers$phone_number))
length(unique(customers$email))
length(unique(customers$Education))
length(unique(customers$Occupation))


# Display the unique values of each attribute
unique(customers$Department.Name)
unique(customers$marital_status)
unique(customers$Province)
unique(customers$Education)
unique(customers$Occupation)