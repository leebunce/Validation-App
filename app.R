# Validation App

# Packages ----------------------------------------------------------------
library(shiny)
library(readr)
library(dplyr)
library(validate)
library(DT)

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
                            DT::dataTableOutput("errors")
                          )
                 ),
                 tabPanel("Notes",
                          h1("Notes"),
                          p("Notes go here."),
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
    
    if (is.null(file())) return(NULL)
    
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
                                            content = function(file) {
                                              write_csv(error_data(), file)})
  
  output$download <- renderUI({
    tryCatch(
      if(!(is.null(error_data()) | nrow(error_data()) == 0)) {
        downloadButton("download_errors", "Download errors")
      },
      error = function(c) NULL
    )
    
  })
    
}

# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)