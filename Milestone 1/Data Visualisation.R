#Data Visualisation
install.packages("ggplot2")
install.packages("reshape2")
install.packages("corrplot")
install.packages("GGally")

library(ggplot2)
library(reshape2)
library(corrplot)

custData <- read.csv('CustData2.csv')

str(custData)
#These columns are numeric:
numericColumns <- custData[,sapply(custData, is.numeric)]
numericColumns
################################################
#Correlation Matrix:
corMatrix <- cor(numericColumns, use = 'complete.obs')
corrplot(corMatrix, method = 'color', tl.col = 'black', tl.cex = 0.8)

################################################
#Pair Plots:

library(GGally)
library(ggplot2)

selectedData <- custData[, c("Annual.Salary", "year_of_birth", "Gross.Year.To.Date...FRS.Contribution", "Gross.Year.To.Date")]

ggpairs(selectedData, 
        title = "Pair Plots:",
        lower = list(continuous = "smooth"), 
        diag = list(continuous = "densityDiag"))

###############################################
#Boxplots

library(ggplot2)
library(scales)

ggplot(custData, aes(x = Department.Name, y = Annual.Salary)) +
  geom_boxplot(fill = 'skyblue', outlier.colour = 'red', outlier.shape = 16, notch = TRUE) +
  labs(title = "Boxplot for Annual Salary by Department", x = 'Department Name', y = 'Annual Salary') +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

###############################################
#Scatter Plots

library(ggplot2)
library(scales)

# Scatter plot for Annual Salary vs Gross Pay Last Paycheck
ggplot(custData, aes(x = Gross.Pay.Last.Paycheck, y = Annual.Salary)) +
  geom_point(color = "skyblue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot: Annual Salary vs Gross Pay Last Paycheck", 
       x = "Gross Pay Last Paycheck", 
       y = "Annual Salary") +
  scale_y_continuous(labels = comma) + 
  theme_minimal()

library(ggplot2)
library(scales)
# Scatter plot for Annual Salary vs Gross Year to Date
ggplot(custData, aes(x = Gross.Year.To.Date, y = Annual.Salary)) +
  geom_point(color = "skyblue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot: Annual Salary vs Gross Year to Date", 
       x = "Gross Year to Date", 
       y = "Annual Salary") +
  scale_y_continuous(labels = comma) +  
  theme_minimal()


################################################
#Histogram

#Histogram for Frequency of Annual Salary
ggplot(custData, aes(x = Annual.Salary)) +
  geom_histogram(binwidth = 5000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Annual Salary", 
       x = "Annual Salary", 
       y = "Frequency") +
  theme_minimal()

#Histogram for Distribution of Birth Years
ggplot(custData, aes(x = year_of_birth)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Birth Years", 
       x = "Years of Birth", 
       y = "Frequency") +
  theme_minimal()


