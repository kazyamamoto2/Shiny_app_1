library(shiny)
library(tidyverse)
library(RSQLite)
library(DBI)

# Define UI for application that draws a histogram
ui <- fluidPage(
        # Application title
        titlePanel(tags$h1("Daily Follow-Up")),
        tabsetPanel(
                tabPanel(tags$h2("Input your data !"), title = "Input", 
                         sidebarLayout(
                                 sidebarPanel(
                         numericInput(inputId = "patientID", label = "Input your Patient ID", value = NULL),
                         dateInput(inputId = "date", label = "Today's date", value = date()),
                         selectInput(inputId = "timing", label = "Select the timing of the day", choices = c("Morning", "Evening")),
                         numericInput(inputId = "fev6",label = "FEV6", value = NULL),
                         numericInput(inputId = "fev1",label = "FEV1", value = NULL),
                         numericInput(inputId = "spo2",label = "SpO2", value = NULL),
                         numericInput(inputId = "weight",label = "Weight", value = NULL),
                         numericInput(inputId = "temperature",label = "Temperature", value = NULL),
                         numericInput(inputId = "pulse",label = "Pulse", value = NULL),
                         numericInput(inputId = "sbp",label = "SBP", value = NULL),
                         numericInput(inputId = "dbp",label = "DBP", value = NULL),
                         actionButton(inputId = "click", label = "Submit!")
                         ),
                                mainPanel(
                                        tableOutput("table")
                                )
                         )
                ),
                tabPanel(tags$h2("Display the change !"), title = "Output",  
        # Sidebar with a slider input for number of bins 
                         sidebarLayout(
                                 sidebarPanel(
                                         numericInput(inputId = "patientID2", label = "Input Patient ID", value = NULL),
                                         dateRangeInput(inputId = "daterange", label = "Select the period", start = Sys.Date()-6, end = Sys.Date(),format = "yyyy-mm-dd"),
                                         checkboxGroupInput(inputId = "mesurement", label = "Choose any measurement",choices = c("FEV1", "SpO2","Weight", "Temperature")),
                                         actionButton(inputId = "click2", label = "Submit!")
                                 ),
                                 
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                         plotOutput("plot")
                                         
                                 )
                         ) )
                    )
   
 )

server <- function(input, output){
        con <- dbConnect(SQLite(), "/Users/kazumichiyamamoto/shiny_app_1/U_Tokyo/U_Tokyo_Project.db")
        df_measurement <- data.frame(dbReadTable(con, "Measurement"))
        dbDisconnect(con)
        
        data <- eventReactive(input$click, {
                con <- dbConnect(SQLite(), "/Users/kazumichiyamamoto/shiny_app_1/U_Tokyo/U_Tokyo_Project.db")
                dbWriteTable(con, "Measurement", data.frame(PatientID = input$patientID, Date = as.Date(input$date, "%y-%m-%d"), Timing_of_the_Day = input$timing, FEV6 = input$fev6, FEV1 = input$fev1, SpO2 = input$spo2, Weight = input$weight, Temperature = input$temperature, Pulse = input$pulse, SBP = input$sbp, DBP = input$dbp, stringsAsFactors = FALSE), append = TRUE)
                data <- dbReadTable(con, "Measurement")
                dbDisconnect(con)
                return(data)
                })
        
        output$table <- renderTable(tail(data(), n=10))
        
       
        
        output$plot <- renderPlot({
                input$click2
                isolate({
                        if(input$mesurement == "FEV1"){
                                df_measurement %>%
                                filter(PatientID == input$patientID2) %>%
                                filter(Date > input$daterange[1] & Date < input$daterange[2] ) %>%
                                ggplot(aes((Date+as.Date("1970-01-01")), FEV1))+
                                geom_line()+
                                xlab("Date")+
                                ylim(0,NA)
                        } else {
                                 df_measurement %>%
                                filter(PatientID == input$patientID2) %>%
                                filter(Date > input$daterange[1] & Date < input$daterange[2] ) %>%
                                ggplot(aes((Date+as.Date("1970-01-01")), SpO2))+
                                geom_line()+
                                xlab("Date")+
                                ylim(c(75,100))
                                }
                        
                })
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

