# Read functions

# Packages ----------------------------------------------------------------
library(readr)
library(lubridate)
library(tools)

# Function ----------------------------------------------------------------

read_mtcars <- function(file){
  
  # Make sure file is the right type
  if(file_ext(file) != 'csv') stop("File must be in csv format.", call. = F)
  
  # Make sure the columns are present and in the right format
  out <- read_csv(file, col_types = cols(
    car = col_character(),
    mpg = col_double(),
    cyl = col_double(),
    disp = col_double(),
    hp = col_double(),
    drat = col_double(),
    wt = col_double(),
    qsec = col_double(),
    vs = col_double(),
    am = col_double(),
    gear = col_double(),
    carb = col_double()
  ))
  
  if(nrow(problems(out)) > 0) stop("Some data was not the expected type.", call. = F)
  
  # Make sure there are no additional columns
  if(ncol(out) != 12) stop("Wrong number of columns.", call. = F)
  
  return(out)
}
