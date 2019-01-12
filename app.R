# Validation App

# Packages ----------------------------------------------------------------
library(shiny)
library(readr)
library(dplyr)
library(validate)
library(DT)

# Options -----------------------------------------------------------------
options(shiny.sanitize.errors = FALSE)

# Read Function -----------------------------------------------------------
source('Read functions/read_mtcars.R')

# Validation rules --------------------------------------------------------
validator_mtcars <- validator(.file = 'Validation rules/mtcars.yaml')
validator_df_mtcars <- validator_mtcars %>% 
  as.data.frame() %>% 
  select(name, label, description)

# UI ----------------------------------------------------------------------
ui <- navbarPage("Validation App",
                 tabPanel("App",
                          sidebarPanel(
                            fileInput("file",
                                      label = "Upload file",
                                      accept = "text/csv"),
                            actionButton("validate", label = "Validate"),
                            uiOutput("download")
                          ),
                          
                          mainPanel(
                            uiOutput("message"),
                            DT::dataTableOutput("errors")
                          )
                 ),
                 tabPanel("Notes",
                          h1("Notes"),
                          p("Data validation is a common task in the collection of data.",
                            "This shiny app provides a template tool for data validation, based around the readr and validate R packages."),
                          p("The user simply uploads the data file they want to validate and the app returns an errors that it finds.",
                            "This is done in two stages.",
                            "First, using the functionality found in readr, the app checks that the data is in the right format and that each column contains the expected data type.",
                            "If there are any problems at this stage these errors will appear in the main panel and be available for download."),
                          p("Once any data errors are resolved the app then tests the data against some pre-defined validation rules.",
                            "This stage makes use of the validate package, and any errors will again be displayed in the main panel and available for download."),
                          p(HTML(paste0("The code used to produce the app can be found ", a(href = "https://github.com/leebunce/Validation-App/", "here"), "."))),
                          h2("Sample data"),
                          p(HTML(paste0("Some sample data for testing the app can be found ", a(href = "https://github.com/leebunce/Validation-App/tree/master/Test%20data", "here"), "."))))
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  file <- eventReactive(input$validate, {
    
    if (is.null(input$file)) return(NULL)
    
    read_mtcars(input$file$datapath)
    
  })
  
  error_data <- reactive({
    
    if(is.null(file())) return(NULL)
    
    if(nrow(problems(file())) > 0) return(problems(file()) %>% select(-file))
    
    confront(file(), validator_mtcars, key = "car") %>% 
      as.data.frame() %>% 
      left_join(validator_df_mtcars, by = 'name') %>% 
      filter(value == F) %>% 
      select(-value, -expression, -label) %>% 
      arrange(car, name) %>% 
      rename(error = name)
  })
  
  output$errors <-  DT::renderDataTable(error_data(), filter = 'top')
  
  output$download_errors <- downloadHandler(filename = "errors.csv",
                                            contentType = "text/csv",
                                            content = function(file) write_csv(error_data(), file))
  
  output$download <- renderUI({
    tryCatch(
      if(!(is.null(error_data()) | nrow(error_data()) == 0)) {
        downloadButton("download_errors", "Download errors", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top: 15px")
      },
      error = function(x) NULL)
  })
  
  output$message <- renderUI({
    tryCatch(
      if(nrow(problems(file())) > 0) h3('Data error(s):', style = "color:red")
      else if(nrow(error_data()) > 0) h3('Validation error(s):', style = "color:red")
      else h3('There are no errors.', style = "color:green"),
      error = function(x) NULL
    )
  })
    
}

# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)