# Validation App

# Packages ----------------------------------------------------------------
library(shiny)
library(readr)
library(dplyr)
library(validate)

# Validation rules --------------------------------------------------------
validator_mtcars <- validator(.file = 'Validation rules/mtcars.yaml')

# UI ----------------------------------------------------------------------
ui <- fluidPage(
   
   # Application title
   titlePanel("Validation App"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("collection",
                     label = "Collection", 
                     choices = list("mtcars")),
         fileInput("file",
                   label = "Upload file",
                   accept = "text/csv"),
         actionButton("validate", label = "Validate")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Data", tableOutput("input_file")),
                    tabPanel("Errors", tableOutput("errors"))
        )
      )
   )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  file <- reactive({
    
    if (is.null(input$file))
      return(NULL)
    
    read_csv(input$file$datapath)
  })
   
  output$input_file <- renderTable({file()})
  
  output$errors <- renderTable({
    if (is.null(file()))
      return(NULL)
    
    confront(file(), validator_mtcars, key = "car") %>% 
      as.data.frame() %>% 
      filter(value == F) %>% 
      arrange(car, name) %>% 
      rename(error = name)
    })
    
}

# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)