####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)


  # Define UI
  ui <- fluidPage(theme = shinytheme("superhero"),
    navbarPage(
      #theme = "cerulean",  # <--- To use a theme, uncomment this
      "LangaSat Service Eligibility Predictor",
      tabPanel("Navbar 1",
               sidebarPanel(
                 tags$h3("Customer Details"),
                 textInput("txtLastName", "Last Name: ", ""),
                 textInput("txtFirstName", "First Name", ""),
                 textInput("txtMiddleInit", "Middle Initial", ""),
                 textInput("txtTitle", "Title", ""),
                 textInput("txtDepartment", "Department", ""),
                 numericInput("txtAnnualSalary", "Annual Salary", ""),
                 numericInput("txtGPLP", "Gross Pay Last Paycheck", ""),
                 numericInput("txtGYTD", "Gross Year To Date", ""),
                 numericInput("txtGYTDFRS", "Gross FRS Contribution", ""),
                 dateInput("txtYearOfBirth", "Date Of Birth", value = Sys.Date()
                           , format = "yyyy", startview = "year"),
                 selectInput("txtMaritalStatus", "Marital Status", 
                             list("Single" = "single",
                                  "Married" = "married",
                                  "Divorced" = "divorced", 
                                  "Widowed" = "widowed")),
                 textAreaInput("txtStreetAddress", "Street Address", ""),
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
                 numericInput("txtHouseholdSize", "Household Size", ""),
                 numericInput("txtYearsInResidance", "Years In Residance", ""),
                 submitButton("Submit")
                 
               ), # sidebarPanel
               mainPanel(
                            h1("Loan Eligibility"),
                            
                            h4("Prediction"),
                            verbatimTextOutput("txtout"),

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Navbar 2", "This panel is intentionally left blank"),
      tabPanel("Navbar 3", "This panel is intentionally left blank")
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    #output$Year_Of_Birth <- {year(input$txtYearOfBirth)}
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
