library(shiny)
library(shinyalert)
library(tidyverse)
library(shinydashboard)


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Mobile Device Data Exploration"),

    # Creating variable selection panel
    sidebarLayout(
      sidebarPanel(
        
        # Categorical Variable selection drop down
        h2("Select Categorical Variable:"),
        
        # Creating drop down for cat variables
        selectizeInput(inputId = "device",
                       label = "Select a Device",
                       choices = c("All", device_data$device), 
                       options = NULL,
                       width = NULL),
        selectizeInput(inputId = "gender",
                       label = "Select a Gender",
                       choices = c("All", device_data$gender), 
                       options = NULL,
                       width = NULL),
        
        # Slider for numerical variables
        h2("Select Numerical Variables"),
        
        selectizeInput(inputId = "num_var1",
                       label = "Select First Variable",
                       options = NULL,
                       widith = NULL),
        
        sliderInput(inputID = "range1",
                    min = 0,
                    max = 2000,
                    value = c(o, 2000)),
        
        selectizeInput(inputId = "num_var2",
                       label = "Select Second Variable",
                       options = NULL,
                       widith = NULL),
        
        sliderInput(inputID = "range2",
                    min = 0,
                    max = 2000,
                    value = c(o, 2000)),
        
        # Action button to update the analysis
        actionButton(inputId = "update",
                     label = "Run Analysis")
      ),
      
      # Creating main panel
      mainPanel(
        tabsetPanel(
          
          # Creating about panel
          tabPanel("About",
                   h2("Purpose of the Mobile Device Usage Analyzer"),
                   p("The Mobile Device Usage Analyzer allows users to explore various variables associated with mobile device usage,
                     and to perform various tasks that allow a user to conduct their own exploratory data anylasis. For more information about
                     the data used go to: https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset "),
                   h3("Explanation of Options"),
                   p("The sidebar allows the user to select a categorical variable from the dataset and then to subset the data based on
                     the various categories within said variable. Then a numerical variable can be selected and subsetted again based on the range
                     of values aviable for that variable"),
                   p("The Data Exploration tab allows the user to explore the realtionship between the variables by providing 
                     numerical and graphical summaries"),
                   p("The Data Download tab allows the user to download the data used in the Data Exploration Tab.")
                   ),
          
          # Creating data download panel
          tabpanel("Data Download",
                   DT::dataTableOutput("subsetted_data"),
                   downloadButton(outputId = "subsetted_data",
                                  label = "Download Data")
                   )
        )
      )
    )
  )

# Define server logic required 
server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
