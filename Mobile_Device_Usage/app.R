library(shiny)
library(shinyalert)
library(tidyverse)
library(shinydashboard)
library(DT)

# Define UI for application that draws a histogram
# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Mobile Device Data Exploration"),
    sidebarLayout(
      sidebarPanel(
 
      
      # Creating drop down for cat variables
      selectInput("cat_var",
                  "Categorical Variable",
                  choices = c("Device", "Gender")
                  ),
      selectInput("level",
                  "Level",
                  choices = NULL),
  
      # Creating slider for numerical variables
      selectInput("num_var",
                  "Numerical Variable 1",
                  choices = num_var),
      sliderInput("range",
                  "Range",
                  0,
                  min = 0,
                  max = 10),
      selectInput("num_var2",
                  "Numerical Variable 2",
                  choices = num_var),
      sliderInput("range2",
                  "Range",
                  0,
                   min = 0,
                  max = 10),
      # Creating action button to save subset
      actionButton("submit", "Submit Subset")
    ),
      
      
  
    # Creating main panel
    mainPanel(
      tabsetPanel(
          
      # Creating about panel
      tabPanel("About",
        h2("Purpose of the Mobile Device Usage Analyzer"),
        p("The Mobile Device Usage Analyzer allows users to explore various variables associated with mobile device usage,and to perform various tasks that allow a user to conduct their own exploratory data anylasis. For more information about the data used go to: https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset "),
        h3("Explanation of Options"),
        p("The sidebar allows the user to select a categorical variable from the dataset and then to subset the data based on the various categories within said variable. Then a numerical variable can be selected and subsetted again based on the range of values aviable for that variable"),
        p("The Data Exploration tab allows the user to explore the realtionship between the variables by providing numerical and graphical summaries"),
        p("The Data Download tab allows the user to download the data used in the Data Exploration Tab.")
        ),
      tabPanel("Data Download",
               DT::dataTableOutput("data_table"),
               downloadButton(outputId = "downloaded_data",
                              label = "Downloaded Data")),
      tabPanel("Data Exploration",
               h2("Select Numerical Variable for Summary"),
               selectInput("num_sum",
                           "Numerical Variable ",
                           choices = num_var),
      
               
               )
      
      )
    )
  )
)


  



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Making sure different numeric variables are selected
  observeEvent(c(input$num_var, input$num_var2), {
    num_var1 <- input$num_var
    num_var2 <- input$num_var2
    choices <- num_var
    if (num_var1 == num_var2) {
      choices <- choices[-which(choices == num_var1)]
      updateSelectInput(session,
                           "num_var2",
                           choices = choices)
    }
  })
  
  # Creating dynamic input for categorical variables
  observeEvent(input$cat_var, {
    cat_var <- input$cat_var
    if(cat_var == "Device") {
      updateSelectInput(inputId = "level",
                        choices = c("All Devices", "Google Pixel 5", "OnePlus 9", "Xiaomi Mi 11", "Samsung Galaxy S21", "iPhone 12"))
    }
    else if(cat_var == "Gender") {
      updateSelectInput(inputId = "level",
                        choices = c("All Genders", "Male", "Female")) 
    }
  })
  
  # Creating dynamic input for numerical variable
  observeEvent(input$num_var, {
    num_var <- input$num_var
    if(num_var == "App Usage (Time)") {
      updateSliderInput(inputId = "range",
                        min = 30,
                        max = 598)
    } else if(num_var == "Screen Time") {
      updateSliderInput(inputId = "range",
                        min = 1,
                        max = 12)
    } else if(num_var == "Battery Drain") {
      updateSliderInput(inputId = "range",
                        min = 302,
                        max = 2993)
    } else if(num_var == "Apps Installed") {
      updateSliderInput(inputId = "range",
                        min = 10,
                        max = 99)
    } else if(num_var == "Data Usage") {
      updateSliderInput(inputId = "range",
                        min = 102,
                        max = 2497)
    } else if(num_var == "Age") {
      updateSliderInput(inputId = "range",
                        min = 18,
                        max = 59)
    }
  })
  observeEvent(input$num_var2, {
    num_var2 <- input$num_var2
    if(num_var2 == "App Usage (Time)") {
      updateSliderInput(inputId = "range2",
                        min = 30,
                        max = 598)
    } else if(num_var2 == "Screen Time") {
      updateSliderInput(inputId = "range2",
                        min = 1,
                        max = 12)
    } else if(num_var2 == "Battery Drain") {
      updateSliderInput(inputId = "range2",
                        min = 302,
                        max = 2993)
    } else if(num_var2 == "Apps Installed") {
      updateSliderInput(inputId = "range2",
                        min = 10,
                        max = 99)
    } else if(num_var2 == "Data Usage") {
      updateSliderInput(inputId = "range2",
                        min = 102,
                        max = 2497)
    } else if(num_var2 == "Age") {
      updateSliderInput(inputId = "range2",
                        min = 18,
                        max = 59)
    }
  })
  
  # Creating a reactive in order to update data by subset
  subsetted_data <- eventReactive(input$submit, {
    sub_data <- device_data
    num1min <- input$range[1]
    num1max <- input$range[2]
    num2min <- input$range2[1]
    num2max <- input$range2[2]
    
    if (input$level == "All Devices") {
      sub_data <- sub_data |>
        select(-user, -OS, -gender, -user_class)
    }
    if (input$cat_var == "All Genders") {
      sub_data <- sub_data |>
        select(-user, -OS, -device, -user_class)
    }
    if (input$cat_var == "Google Pixel 5") {
      sub_data <- sub_data |>
        select(-user, -OS, -gender, -user_class) |>
        filter(device == "Google Pixel 5")
    }
    if (input$cat_var == "OnePlus 9") {
      sub_data <- sub_data |>
        select(-user, -OS, -gender, -user_class) |>
        filter(device == "OnePlus 9")
    }
    if (input$cat_var == "Xiaomi Mi 11") {
      sub_data <- sub_data |>
        select(-user, -OS, -gender, -user_class) |>
        filter(device == "Xiaomi Mi 11")
    }
    if (input$cat_var == "Samsung Galaxy S21") {
      sub_data <- sub_data |>
        select(-user, -OS, -gender, -user_class) |>
        filter(device == "Samsung Galaxy S21")
    }
    if (input$cat_var == "iPhone 12") {
      sub_data <- sub_data |>
        select(-user, -OS, -gender, -user_class) |>
        filter(device == "iPhone 12")
    }
    if (input$cat_var == "Male") {
      sub_data <- sub_data |>
        select(-user, -OS, -device, -user_class)
    }
    if (input$cat_var == "Female") {
      sub_data <- sub_data |>
        select(-user, -OS, -device, -user_class)
    }
    
    return(sub_data)
  })
  

  
  # Rendering the data table for tab 2 from filtered_data subset above
  output$data_table <- DT::renderDataTable({
    subsetted_data()
  })
  
  # Download handler for the filtered data as a csv. file. for button in tab 2
  output$download_data <- downloadHandler(
    filename = function() {
      paste('subsetted_data', Sys.Date(), '.csv', sep ='')
    },
    content = function(file) {
      write.csv(subsetted_data(), file)
    }
  )
    

  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
