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
               textInput("txt1", "Given Name:", ""),
               textInput("txt2", "Surname:", ""),
              
               
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
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
