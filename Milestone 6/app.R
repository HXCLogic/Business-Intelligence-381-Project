library(shiny)
library(shinythemes)

custData <- read.csv("CustData2.csv")
# Load the model and preprocessing pipeline (assuming they are saved as RDS files)
random_forest_model <- readRDS("random_forest_model.rds")
preprocessing_pipeline <- readRDS("preprocessing_pipeline.rds")

# Pre-calculate frequency tables (replace with your actual data)
title_frequency <- table(custData$Title)
department_frequency <- table(custData$Department.Name)
country_frequency <- table(custData$Country_id)

# Define UI
ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage(
    "LangaSat Service Eligibility Predictor",
    tabPanel(
      "Customer Details",
      sidebarPanel(
        tags$h3("Customer Details"),
        textInput("txtTitle", "Title", ""),
        textInput("txtDepartment", "Department", ""),
        numericInput("txtAnnualSalary", "Annual Salary", value = 50000),  # Set a default value
        numericInput("txtGPLP", "Gross Pay Last Paycheck", value = 0),  # Set a default value
        numericInput("txtGYTD", "Gross Year To Date", value = 0),  # Set a default value
        numericInput("txtGYTDFRS", "Gross FRS Contribution", value = 0),  # Set a default value
        dateInput("txtYearOfBirth", "Date Of Birth", value = Sys.Date(), format = "yyyy", startview = "year"),
        selectInput("txtMaritalStatus", "Marital Status",
                    list("Single" = "single",
                         "Married" = "married",
                         "Divorced" = "divorced",
                         "Widowed" = "widowed")),
        textInput("txtStreetAddress", "Street Address", ""),
        textInput("txtPostalCode", "Postal Code", ""),
        textInput("txtCity", "City", ""),
        textInput("txtState", "State", ""),
        textInput("txtProvince", "Province", ""),
        textInput("txtCountryID", "Country ID", ""),
        textInput("txtPhoneNumber", "Phone Number", ""),
        textInput("txtEmail", "Email", ""),
        selectInput("txtEducation", "Education",
                    list("Bachelor's Degree" = "Bach.",
                         "Masters" = "Masters",
                         "High School" = "HS-grad")),
        selectInput("txtOccupation", "Occupation",
                    list("Cleric" = "Cleric.",
                         "Professor" = "Prof.",
                         "Executive" = "Exec.",
                         "Sales" = "Sales")),
        numericInput("txtHouseholdSize", "Household Size", value = 1),  # Set a default value
        numericInput("txtYearsInResidance", "Years In Residance", value = 0),  # Set a default value
        submitButton("Submit")
      )
    ),
    tabPanel("Navbar 2", "This panel is intentionally left blank"),
    tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
)

# Define server function
server <- function(input, output) {
  # Reactive function to preprocess the new record
  preprocessed_record <- reactive({
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
    
    preprocess_record(new_record, title_frequency, department_frequency, country_frequency)
  })
  
  # Reactive function to make the prediction
  prediction <- reactive({
    if (!is.null(preprocessed_record())) {
      predict(random_forest_model, newdata = preprocessed_record()$record, type = "response")
    } else {
      return(NA)
    }
  })
  
  # Output the prediction with appropriate message
  output$txtout <- renderText({
    if (!is.null(preprocessed_record())) {
      print(preprocessed_record())  # Print preprocessed record
      prediction <- predict(random_forest_model, newdata = preprocessed_record()$record, type = "response")
      print(prediction)  # Print prediction
      predVal <- ifelse(prediction == "0.743001202264081", "Eligible", "Not Eligible")
      paste("Predicted Eligibility:", predVal)
    } else {
      predVal <- ifelse(prediction() == "0.743001202264081", "Eligible", "Not Eligible")
      paste("Predicted Eligibility:", predVal)
    }
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)