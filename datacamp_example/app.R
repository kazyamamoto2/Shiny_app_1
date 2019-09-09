library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
        sidebarLayout(
                sidebarPanel(
                        fileInput(inputId = "data_file", label = "Choose CSV File",
                                  accept = ".csv", buttonLabel = "Browse...",
                                  placeholder = "No file selected"),
                       
                         sliderInput(inputId = "date_range",label = "Select period", value = c(1990, 2000),min = 1970,
                                     max = 2020),
                       
                         checkboxGroupInput(inputId = "country", label = "choose country",choices = c("Japan", "Republic of Korea","United States of America", "France"))
                        
                ),
                mainPanel(plotOutput(outputId = "plot"))
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        #uploaded data
        csv_file <- reactive({df <- read_csv(input$data_file$datapath)
        #mutate the rate of suicide
        df <- df %>%
                mutate(percentage = suicides_no/population)
        # reorder the level
        df$age <- factor(df$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))
        # filter and group 4 countries
        df_filtered <- df %>%
                filter(country == input$country ) %>%
                filter(year > input$date_range[1] & year < input$date_range[2]) %>%
                group_by(country, year) %>%
                summarize(total_no = sum(suicides_no),
                          total_pop = sum(population),
                          no_per_100000 = total_no/total_pop*100000)
       })
        
        output$plot <- renderPlot({
                # yearly change among countries
                ggplot(csv_file(), aes(x=year, y=no_per_100000, color = country))+
                        geom_point()+
                        geom_line()
                       })
}

# Run the application 
shinyApp(ui = ui, server = server)
