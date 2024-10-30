# Load necessary libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(caret)
library(lubridate)
library(randomForest)

# Load the saved model and preprocessing pipeline
random_forest_model <- readRDS("random_forest_model.rds")
preprocessing_pipeline <- readRDS("preprocessing_pipeline.rds")

importance_plot <- varImpPlot(random_forest_model)

#Load the accuracy Metrics
metrics <- read.csv("accuracy_metrics.csv")

# Import frequency encoding attributes
title_frequency <- read.csv("title_frequency.csv")
department_frequency <- read.csv("department_frequency.csv")
country_frequency <- read.csv("country_frequency.csv")

# Define UI
ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage(
    "LangaSat Service Eligibility Predictor",
    tabPanel(
      "Customer Details",
      sidebarPanel(
        width = 7, 
        tags$h3("Customer Details"),
        fluidRow(
          column(6,
                 textInput("txtTitle", "Title", "", placeholder = "Title"),
                 textInput("txtDepartment", "Department", "", placeholder = "Department"),
                 numericInput("txtAnnualSalary", "Annual Salary", value = 0,),
                 numericInput("txtGPLP", "Gross Pay Last Paycheck", value = 0),
                 numericInput("txtGYTD", "Gross Year To Date", value = 0),
                 numericInput("txtGYTDFRS", "Gross FRS Contribution", value = 0),
          ),
          column(6,
                 dateInput("txtYearOfBirth", "Date Of Birth", value = Sys.Date(), format = "yyyy", startview = "year"),
                 selectInput("txtMaritalStatus", "Marital Status",
                             list("Single" = "single", "Married" = "married", "Divorced" = "divorced", "Widowed" = "widowed")),
                 textInput("txtCountryID", "Country ID", "", placeholder = "Country ID"),
                 selectInput("txtEducation", "Education",
                             list("Bachelor's Degree" = "Bach.", "Masters" = "Masters", "High School" = "HS-grad")),
                 selectInput("txtOccupation", "Occupation",
                             list("Cleric" = "Cleric.", "Professor" = "Prof.", "Executive" = "Exec.", "Sales" = "Sales")),
                 numericInput("txtHouseholdSize", "Household Size", value = 0),
                 numericInput("txtYearsInResidance", "Years In Residence", value = 0)
          )
        ),
        actionButton("submit", "Submit")
      ),
      mainPanel(
        width = 4,
        h1("Customer Eligibility"),
        textOutput("txtout")
      )
    ),
    tabPanel("Model Accuracy Metrics", 
             h1("Model Accuracy Metrics"),
             textOutput("txtAccuracy"),
             textOutput("txtPrecision"),
             textOutput("txtRecall"),
             textOutput("txtF1"),
             textOutput("txtOriginal"),
             textOutput("txtImproved"),
             textOutput("txtIncrease"),
             h2("Feature Importance"),
             plotOutput("txtPlot")
             
    ),
    tabPanel("Development Team", 
             h1("Development Team"),
             h4("Jo-Anne van der Wath"),
             h4("Henry Roux"),
             h4("Armandre Erasmus"),
             h4("Chaleigh Storm"))
  )
)

# Define server function
server <- function(input, output) {
  # Reactive function to preprocess the new record
  preprocessed_record <- reactive({
    req(input$submit)
    
    # Create new record from input values
    new_record <- data.frame(
      Title = input$txtTitle,
      Department.Name = input$txtDepartment,
      Annual.Salary = input$txtAnnualSalary,
      Gross.Pay.Last.Paycheck = input$txtGPLP,
      Gross.Year.To.Date = input$txtGYTD,
      Gross.Year.To.Date...FRS.Contribution = input$txtGYTDFRS,
      year_of_birth = as.integer(year(input$txtYearOfBirth)),
      marital_status = input$txtMaritalStatus,
      Country_id = input$txtCountryID,
      Education = input$txtEducation,
      Occupation = input$txtOccupation,
      household_size = input$txtHouseholdSize,
      yrs_residence = input$txtYearsInResidance
    )
    
    # Calculate 'Age'
    new_record <- new_record %>%
      mutate(Age = as.integer(year(Sys.Date()) - year_of_birth))
    
    new_record$Eligible <- 1
    
    # Encoding (frequency encoding using pre-calculated tables)
    new_record$Frequency_Title <- ifelse(new_record$Title %in% title_frequency$Title, title_frequency[new_record$Title], 0)
    new_record$Frequency_Department <- ifelse(new_record$Department.Name %in% department_frequency$Department, department_frequency[new_record$Department.Name], 0)
    new_record$Frequency_Country_ID <- ifelse(new_record$Country_id %in% country_frequency$Country, country_frequency[new_record$Country_id], 0)
    
    # One-hot encode marital status, education, and occupation
    new_record <- new_record %>%
      mutate(
        Marital_Status_married = ifelse(marital_status == "married", 1, 0),
        Marital_Status_single = ifelse(marital_status == "single", 1, 0),
        Marital_Status_divorced = ifelse(marital_status == "divorced", 1, 0),
        Marital_Status_widowed = ifelse(marital_status == "widowed", 1, 0),
        Education_Bach = ifelse(Education == "Bach.", 1, 0),
        Education_Masters = ifelse(Education == "Masters", 1, 0),
        Education_HS = ifelse(Education == "HS-grad", 1, 0),
        Occupation_Cleric = ifelse(Occupation == "Cleric.", 1, 0),
        Occupation_Prof = ifelse(Occupation == "Prof.", 1, 0),
        Occupation_Exec = ifelse(Occupation == "Exec.", 1, 0),
        Occupation_Sales = ifelse(Occupation == "Sales", 1, 0)
      )
    
    # Set minimum value threshold for Box-Cox compatibility
    new_record <- new_record %>%
      mutate(across(where(is.numeric), ~ ifelse(. <= 0, 0.001, .)))
    
    # Ensure new_record has all columns required by the preprocessing pipeline
    required_columns <- names(preprocessing_pipeline$mean)
    new_record <- new_record %>% select(all_of(required_columns))
    
    # Convert columns to numeric if necessary
    new_record <- new_record %>% mutate(across(everything(), as.numeric))
    
    # Apply preprocessing pipeline
    processed_record <- predict(preprocessing_pipeline, newdata = new_record)
    return(processed_record)
  })
  
  
  # Reactive function to make the prediction
  prediction <- eventReactive(input$submit, {
    predict(random_forest_model, newdata = preprocessed_record(), type = "response")
  })
  
  # Output the prediction
  output$txtout <- renderText({
    req(prediction())
    predVal <- ifelse(prediction() == "0.743001202264081", "Eligible", "Not Eligible")
    paste(predVal)
  })
  
  #Output accuracy
  output$txtAccuracy <- renderText({
    accuracy <- metrics$Accuracy
    paste("Accuracy:", accuracy, "%")
  })
  
  #Output precision
  output$txtPrecision <- renderText({
    precision <- metrics$Precision
    paste("Precision:", precision, "%")
  })
  
  #Output recall
  output$txtRecall <- renderText({
    recall <- metrics$Recall
    paste("Recall:", recall, "%")
  })
  
  #Output f1 score
  output$txtF1 <- renderText({
    f1 <- metrics$F1_Score
    paste("F1 Score:", f1, "%")
  })
  
  #Output original eligibility rate
  output$txtOriginal <- renderText({
    original <- round(metrics$original_eligibility_rate,2)
    paste("Rate of Eligibility of Baseline Model:", original, "%")
  })
  
  #Output improved eligibility rate
  output$txtImproved <- renderText({
    improved <- round(metrics$improved_eligibility_rate,2)
    paste("Rate of Eligibility of Random Forest Model:", improved, "%")
  })
  
  #Output increase in eligible customers
  output$txtIncrease <- renderText({
    increase <- round(metrics$improved_eligibility_rate - metrics$original_eligibility_rate,2)
    paste("Increase in the Number of Eligible Customers:", increase, "%")
  })
  
  #Output feature importance plot
  output$txtPlot <- renderPlot({
    importance_plot <- varImpPlot(random_forest_model)
    paste(importance_plot)
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)
