library(shiny)
library(ggplot2)
rf_model <- readRDS('rfModel.rds')

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Credit Card Fraud Prediction"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
  
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  mydata <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <-read.csv(input$file1$datapath,
                    stringsAsFactors = TRUE)
    
    #print(rf_model)
    data$predictions<-as.integer(as.character(predict(rf_model, data)))
    return(data)
    
    
    # Predict Credit Card Fraud by Random Forest Model 
    
  })    
  output$distPlot <- renderPlot({

    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    fraud_cases <- sum(mydata()$predictions==1)
    non_fraud_cases <- sum(mydata()$predictions==0)
    fraud_cases_perc <- fraud_cases/nrow(mydata())*100
    non_fraud_cases_perc <- non_fraud_cases/nrow(mydata())*100
    fraud_data <- data.frame(Type = c("Fraud","Non-Fraud"), Count = c(fraud_cases,non_fraud_cases), Percentage = c(fraud_cases_perc,non_fraud_cases_perc))
    ggplot(fraud_data, aes(x = Type, y = Count, fill = Type)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Count, " (", sprintf("%.1f", Percentage), "%)"), y = Count), size = 3, vjust = -0.3, position = position_dodge(.9)) +
      labs(title = "Distribution of Fraud Cases", x = "Type", y = "Count") +
      theme(plot.title = element_text(hjust = 0.5))

  })
  
  output$contents <- renderTable({
    req(input$file1)
    
    mydata()
    

  })


}

# Create Shiny app ----
shinyApp(ui, server)