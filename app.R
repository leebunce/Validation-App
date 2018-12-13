# Validation App

# Packages ----------------------------------------------------------------
library(shiny)
library(readr)
library(validate)

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
        tableOutput("input_file")
      )
   )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
   
  output$input_file <- renderTable({
    
    input_file <- input$file
    
    if (is.null(input_file))
      return(NULL)
    
    read_csv(input_file$datapath)
  })
    
}

# Run ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)