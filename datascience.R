# Load necessary libraries
install.packages(c("readr", "dplyr", "shiny", "ggplot2", "arules"))
library(readr)
library(dplyr)
library(shiny)
library(ggplot2)
library(arules)

## Define UI for application
ui <- fluidPage(
  titlePanel("CSV Data Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      numericInput("n_clusters", "Number of Clusters", value = 2, min = 2, max = 4),
      numericInput("min_supp", "Minimum Support", value = 0.001, min = 0.001, max = 1),
      numericInput("min_conf", "Minimum Confidence", value = 0.001, min = 0.001, max = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data View", tableOutput("contents")),
        tabPanel("Data Summary", verbatimTextOutput("summary")),
        tabPanel("Data Analysis", 
                 plotOutput("plot1"),
                 plotOutput("plot2"),
                 plotOutput("plot3"),
                 plotOutput("plot4")),
        tabPanel("Clustering Results", verbatimTextOutput("clusters"), plotOutput("plot5")),
        tabPanel("Association Rules", verbatimTextOutput("rules"))
      )
    )
  )
)

## Define server logic
server <- function(input, output) {
  inFile <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- read.csv(inFile$datapath, header = input$header)
    
    if(any(is.na(df))){
      df <- df[complete.cases(df), ]  
    }
    
    if(any(duplicated(df))){
      df <- df[!duplicated(df), ]  
    }
    
    df
  })
  
  output$contents <- renderTable({
    inFile()
  })
  
  output$summary <- renderPrint({
    df <- inFile()
    str(df)
    print(paste("Number of duplicated rows: ", sum(duplicated(df))))
    print(paste("Number of missing values: ", sum(is.na(df))))
  })
  
  output$plot1 <- renderPlot({
    df <- inFile()
    if ("paymentType" %in% colnames(df)) {
      x <- table(df$paymentType)
      percentage <- paste0(round(100*x/sum(x)),"%")
      pie(x, labels = percentage, main = "Comparing Payment Method", col = c("lightblue","purple"))
      legend("bottomright", legend = c("cash","credit"), fill = c("lightblue","purple"))
    }
  })
  
  output$plot2 <- renderPlot({
    df <- inFile()
    if ("age" %in% colnames(df) & "total" %in% colnames(df)) {
      age_spendingtotal <- df %>% group_by(age) %>% summarise(total_spending = sum(total), .groups = "drop")
      ggplot(age_spendingtotal, aes(x = age, y = total_spending)) +
        geom_bar(stat = "identity") +
        labs(title = "Total Spending by Age Category", x = "Age Category", y = "Total Spending")
    }
  })
  
  output$plot3 <- renderPlot({
    df <- inFile()
    if ("city" %in% colnames(df) & "total" %in% colnames(df)) {
      grouped_total <- df %>% group_by(city) %>% 
        summarise(totalspending = sum(total), .groups = "drop")
      ndf <- grouped_total[order(grouped_total$totalspending , decreasing = TRUE),]
      barplot(ndf$totalspending , names.arg = ndf$city ,
              xlab = "city" , ylab = "total spending" , main = "display each city total spending",
              col="lightpink")
    }
  })
  
  output$plot4 <- renderPlot({
    df <- inFile()
    if ("total" %in% colnames(df)) {
      boxplot(df$total,
              main = "Distribution of Total Spending",
              ylab = "Total Spending",
              col = "maroon",
              border = "black"
      )
      
    }
  })
  
  output$clusters <- renderPrint({
    req(input$n_clusters)
    df <- inFile()
    if (all(c("customer", "total", "age") %in% colnames(df))) {
      df2 <- df %>% group_by(customer, age) %>% summarise(total = sum(total), .groups = "drop")
      Kmean_clustering <- kmeans(df2[, c("total", "age")], centers = input$n_clusters)
      df2$cluster <- Kmean_clustering$cluster
      print(df2)
    } else {
      print("Columns 'customer', 'total', and 'age' not found in the data.")
    }
  })
  
  output$plot5 <- renderPlot({
    df <- inFile()
    if (all(c("customer", "total", "age") %in% colnames(df))) {
      df2 <- df %>% group_by(customer, age) %>% summarise(total = sum(total), .groups = "drop")
      Kmean_clustering <- kmeans(df2[, c("total", "age")], centers = input$n_clusters)
      df2$cluster <- Kmean_clustering$cluster
      ggplot(df2, aes(x = total, y = age, color = as.factor(cluster))) +
        geom_point() +
        labs(title = "Scatter Plot of Clusters", x = "Total Spending", y = "Age", color = "Cluster")
    }
  })
  
  output$rules <- renderPrint({
    validate(
      need(as.numeric(input$min_supp) >= 0.001 && as.numeric(input$min_supp) <= 1, "Minimum Support must be between 0.001 and 1."),
      need(as.numeric(input$min_conf) >= 0.001 && as.numeric(input$min_conf) <= 1, "Minimum Confidence must be between 0.001 and 1.")
    )
    
    df <- inFile()
    if (all(c("customer", "items", "rnd", "city", "total", "paymentType", "age") %in% colnames(df))) {
      df <- unique(df)
      itemscol <- df[,1]
      temp <- tempfile()
      write(itemscol, file = temp, sep = ",")
      transactions <- read.transactions(temp, format = "basket", sep = ",")
      transactions <- unique(transactions)  
      apriori_rules <- apriori(transactions, parameter = list(supp = as.numeric(input$min_supp), conf = as.numeric(input$min_conf), minlen = 2, maxtime = 100))  
      print(apriori_rules)
      inspect(apriori_rules)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)