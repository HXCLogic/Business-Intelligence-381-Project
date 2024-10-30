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
        tags$h3("Customer Details"),
        textInput("txtTitle", "Title", "CORRECTIONAL OFFICER"),
        textInput("txtDepartment", "Department", "CORRECTIONS & REHABILITATION"),
        numericInput("txtAnnualSalary", "Annual Salary", value = 54619.76),
        numericInput("txtGPLP", "Gross Pay Last Paycheck", value = 2501.62),
        numericInput("txtGYTD", "Gross Year To Date", value = 48025.48),
        numericInput("txtGYTDFRS", "Gross FRS Contribution", value = 46616.58),
        dateInput("txtYearOfBirth", "Date Of Birth", value = Sys.Date(), format = "yyyy", startview = "year"),
        selectInput("txtMaritalStatus", "Marital Status",
                    list("Single" = "single", "Married" = "married", "Divorced" = "divorced", "Widowed" = "widowed")),
        textInput("txtCountryID", "Country ID", ""),
        selectInput("txtEducation", "Education",
                    list("Bachelor's Degree" = "Bach.", "Masters" = "Masters", "High School" = "HS-grad")),
        selectInput("txtOccupation", "Occupation",
                    list("Cleric" = "Cleric.", "Professor" = "Prof.", "Executive" = "Exec.", "Sales" = "Sales")),
        numericInput("txtHouseholdSize", "Household Size", value = 1),
        numericInput("txtYearsInResidance", "Years In Residence", value = 0),
        actionButton("submit", "Submit")
      ),
      mainPanel(
        textOutput("txtout")
      )
    ),
    tabPanel("Navbar 2", "This panel is intentionally left blank"),
    tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
)

# Define server function
server <- function(input, output) {
  # Reactive function to preprocess the new record
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
    paste("Predicted Eligibility:", predVal)
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)
