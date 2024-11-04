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

append_new_record <- function(new_record) {
  # Read existing records from CSV
  existing_records <- read.csv("new_records.csv")
  
  # Append the new record to the existing records
  updated_records <- rbind(existing_records, new_record)
  
  # Write updated records back to CSV
  write.csv(updated_records, "new_records.csv", row.names = FALSE)
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("superhero"),
  tags$style(HTML("
    /* Table background */
    .dataTable { 
      background-color: white !important;
      color: black;
    }
    
    /* Search box */
    .dataTables_wrapper .dataTables_filter input {
      background-color: white !important;
      color: black;
      border: 1px solid #ccc;
    }
    
    /* Pagination buttons */
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      background-color: white !important;
      color: black !important;
    }
    
    /* Pagination button on hover */
    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background-color: #ddd !important;
      color: black !important;
    }
    
    /* Change the color of the 'Show entries' label and dropdown */
    .dataTables_length label, .dataTables_length select {
    color: white;
    }

    /* Change the color of the pagination information text */
    .dataTables_info {
    color: white;
    }

    /* Change the color of the pagination controls (Previous, Next, page numbers) */
    .dataTables_paginate a,
    .dataTables_paginate span {
    color: white !important;
    }

    /* Change the color of the 'Search' label */
    .dataTables_filter label {
    color: white;
    } 

  ")),
  navbarPage(
    "LangaSat Service Eligibility Predictor",
    tabPanel(
      "Customer Details",
      sidebarPanel(
        width = 7, 
        tags$h3("Customer Details"),
        fluidRow(
          column(6,
                 textInput("txtName", "First Name", "", placeholder = "First Name"),
                 textInput("txtSurname", "Surname", "", placeholder = "Surname"),
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
    tabPanel("Bulk Predictions", 
             h1("Please Upload File for Prediction Analysis"),
             fileInput("txtfile", "Please select a file for analysis"),
             actionButton("bulkSubmit", "Submit"),
             DT::dataTableOutput("table")
    ),
    tabPanel("View Records", 
             h1("All Processed Records"),
             actionButton("allView", "View all Records"),
             DT::dataTableOutput("RecordsTable"),
             h2("New Records"),
             textOutput("txtTotalNewRecords"),
             textOutput("txtEligibleNewRecords"),
             textOutput("txtNonEligibleNewRecords"),
             textOutput("txtPercentageEligibleNewRecords"),
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
  #Get the new record
  get_record <- eventReactive(input$submit, {
    new_record <- data.frame(
      Last.Name = input$txtSurname, 
      First.Name = input$txtName,
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
    return(new_record)
  })
  
  # Reactive function to preprocess the new record
  preprocessed_record <- reactive({
    req(input$submit)
    
    # Create new record from input values
    new_record <- get_record()
    
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
  
  ################################################################

  # Process and predict for each row in the uploaded file
  # Bulk processing function
  bulk_preprocess_and_predict <- function(data) {
    # Loop through each row in the uploaded CSV file
    processed_data <- lapply(1:nrow(data), function(i) {
      # Extract the i-th row as a new record
      new_record <- data[i, ]
      
      # Preprocess the new record
      preprocessed_record <- new_record %>%
        mutate(
          Age = as.integer(year(Sys.Date()) - new_record$year_of_birth),
          Eligible = 1,
          Frequency_Title = ifelse(new_record$Title %in% title_frequency$Title, title_frequency[new_record$Title], 0),
          Frequency_Department = ifelse(new_record$Department.Name %in% department_frequency$Department, department_frequency[new_record$Department.Name], 0),
          Frequency_Country_ID = ifelse(new_record$Country_id %in% country_frequency$Country, country_frequency[new_record$Country_id], 0),
          Marital_Status_married = ifelse(new_record$marital_status == "married", 1, 0),
          Marital_Status_single = ifelse(new_record$marital_status == "single", 1, 0),
          Marital_Status_divorced = ifelse(new_record$marital_status == "divorced", 1, 0),
          Marital_Status_widowed = ifelse(new_record$marital_status == "widowed", 1, 0),
          Education_Bach = ifelse(new_record$Education == "Bach.", 1, 0),
          Education_Masters = ifelse(new_record$Education == "Masters", 1, 0),
          Education_HS = ifelse(new_record$Education == "HS-grad", 1, 0),
          Occupation_Cleric = ifelse(new_record$Occupation == "Cleric.", 1, 0),
          Occupation_Prof = ifelse(new_record$Occupation == "Prof.", 1, 0),
          Occupation_Exec = ifelse(new_record$Occupation == "Exec.", 1, 0),
          Occupation_Sales = ifelse(new_record$Occupation == "Sales", 1, 0)
        ) %>%
        mutate(across(where(is.numeric), ~ ifelse(. <= 0, 0.001, .))) %>%
        select(all_of(names(preprocessing_pipeline$mean))) %>%
        mutate(across(everything(), as.numeric))

      # Apply the preprocessing pipeline
      processed_record <- predict(preprocessing_pipeline, newdata = preprocessed_record)
      
      # Make the prediction
      pred <- predict(random_forest_model, newdata = processed_record, type = "response")
      predVal <- ifelse(pred == "0.743001202264081", "Eligible", "Not Eligible")
      
      # Add the prediction to the new record
      new_record$Eligible <- predVal
      
      new_record <- new_record %>% select(-Column1, -Middle.Initial,
                                          -street_address, -postal_code, -city,
                                          -State, -Province, -phone_number, 
                                          -email)
      
      return(new_record)
    })
    
    # Convert list back to data frame
    processed_data <- do.call(rbind, processed_data)
    return(processed_data)
  }
  
  # Reactive function for bulk predictions
  input_file <- reactive({
    req(input$bulkSubmit)
    if (is.null(input$txtfile)) {
      return(NULL)
    }
    # Read the uploaded CSV file
    data <- read.csv(file = input$txtfile$datapath)
    
    # Process and predict for bulk data
    bulk_predictions <- bulk_preprocess_and_predict(data)
    
    # for (i in nrow(bulk_predictions)) {
    #   append_new_record(bulk_predictions[i, ])
    # }
    append_new_record(bulk_predictions)
    
    return(bulk_predictions)
  })
  
  # Display bulk predictions
  output$table <- DT::renderDataTable({
    req(input_file())
    input_file()
  })
  
  # Display all new records
  output$RecordsTable <- DT::renderDataTable({
    req(input$allView)
    data <- read.csv("new_records.csv")
    data
  })
  
  #################################################################
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
  
  observeEvent(input$submit, {
    new_record <- get_record()
    predVal <- ifelse(prediction() == "0.743001202264081", "Eligible", "Not Eligible")
    new_record$Eligible <- predVal
    append_new_record(new_record)
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
  
  #Output total new records
  output$txtTotalNewRecords <- renderText({
    records <- read.csv("new_records.csv")
    CountCustomers <- nrow(records)
    paste("Total amount of customer predictions:", CountCustomers)
  })
  
  #Output eligible new records
  output$txtEligibleNewRecords <- renderText({
    records <- read.csv("new_records.csv")
    EligibleCustomers <- nrow(records[records$Eligible == "Eligible", ])
    paste("Total amount of customers that are eligible:", EligibleCustomers)
  })
  
  #Output non-eligible new records
  output$txtNonEligibleNewRecords <- renderText({
    records <- read.csv("new_records.csv")
    NonEligibleCustomers <- nrow(records[records$Eligible == "Not Eligible", ])
    paste("Total amount of customers that are not eligible:", NonEligibleCustomers)
  })
  
  #Output non-eligible new records
  output$txtPercentageEligibleNewRecords <- renderText({
    records <- read.csv("new_records.csv")
    CountCustomers <- nrow(records)
    EligibleCustomers <- nrow(records[records$Eligible == "Eligible", ])
    PercentageNewEligibleCustomers <- round(EligibleCustomers / CountCustomers * 100, 2)
    paste("Total amount of customers that are not eligible:", PercentageNewEligibleCustomers, " %")
  })
  
  #Output feature importance plot
  output$txtPlot <- renderPlot({
    importance_plot <- varImpPlot(random_forest_model)
    paste(importance_plot)
  })
  
}

# Create Shiny app
shinyApp(ui = ui, server = server)
