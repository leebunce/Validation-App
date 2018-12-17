# Validation App

# Packages ----------------------------------------------------------------
library(shiny)
library(readr)
library(dplyr)
library(validate)

# Validation rules --------------------------------------------------------
validator_mtcars <- validator(.file = 'Validation rules/mtcars.yaml')
validator_df_mtcars <- validator_mtcars %>% 
  as.data.frame() %>% 
  select(name, label, description)

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
                    tabPanel("Errors", tableOutput("errors")),
                    tabPanel("Data", tableOutput("data"))
        )
      )
   )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  file <- eventReactive(input$validate, {
    
    if (is.null(input$file))
      return(NULL)
    
    read_csv(input$file$datapath)
  })
   
  output$data <- renderTable({file()})
  
  output$errors <- renderTable({
    if (is.null(file()))
      return(NULL)
    
    confront(file(), validator_mtcars, key = "car") %>% 
      as.data.frame() %>% 
      left_join(validator_df_mtcars, by = 'name') %>% 
      filter(value == F) %>% 
      select(-value) %>% 
      arrange(car, name) %>% 
      rename(error = name)
    })
    
}

# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)