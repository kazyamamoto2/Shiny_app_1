library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
        sidebarLayout(
                sidebarPanel(
                        fileInput(inputId = "data_file", label = "Choose CSV File",
                                  accept = ".csv", buttonLabel = "Browse...",
                                  placeholder = "No file selected"),
                        dateRangeInput(inputId = "date_range", label = "Period", start = NULL, end = NULL, min = NULL,
                                       max = NULL, format = "yyyy-mm-dd", startview = "day", weekstart = 0,
                                       language = "en", separator = " to ", width = NULL),
                        checkboxGroupInput(inputId = "country", label = "choose country",choices = c("Japan", "Republic of Korea","United States of America", "France"))
                        
                ),
                mainPanel(plotOutput(outputId = "plot"))
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        "hello"
}

# Run the application 
shinyApp(ui = ui, server = server)
