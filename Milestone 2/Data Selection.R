# ** Correlation **
# Load the dataset
customers <- read.csv("CustData2.csv")

# Select numerical attributes
numeric_data <- customers[sapply(customers, is.numeric)]

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print(correlation_matrix)

# Plot correlation matrix
# install.packages("ggplot2") ## Install package if not already done
library(ggplot2)
# install.packages("reshape2") ## Install package if not already done
library(reshape2)

# Convert to long format for ggplot
melted_corr_matrix <- melt(correlation_matrix)
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Matrix", x = "Attributes", y = "Attributes")


# ** Cardinality **
# Load the dataset
customers <- read.csv("CustData2.csv")

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

# Optional: Sort the results by cardinality to easily identify attributes with high or low cardinality
cardinality_df <- cardinality_df[order(-cardinality_df$Cardinality),]

# Print the sorted cardinality dataframe
print(cardinality_df)


# ** Data Quality **
## Missing Data
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


## numerical attributes
numeric_data <- customers[sapply(customers, is.numeric)]
summary_stats <- summary(numeric_data)
print("Summary statistics for numerical attributes (use to detect outliers):")
print(summary_stats)

# Detect outliers using the IQR method for numerical attributes
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(sum(x < lower_bound | x > upper_bound, na.rm = TRUE))
}

outliers <- sapply(numeric_data, detect_outliers)
outliers_df <- data.frame(Attribute = names(outliers), Outlier_Count = outliers)
print("Outliers detected in numerical attributes:")
print(outliers_df) 

