
library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(plyr)
library(tools)



#Author: Yvonne Li 
##Data Source: https://data.world/doe/us-distribution-production-oil   oil production data

oil <- read.csv("oildata.csv",fileEncoding="UTF-8-BOM",header=TRUE, check.names = FALSE)



# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("num_oil_wells", "oil_prod_BBL","oil_wells_dayson","ADgas_prod_MCF","NAgas_prod_MCF"), 
                  selected = "oil_prod_BBL"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("rate_class", "prod_year"), 
                  selected = "rate_class"),
      
      # Select variable for color -----------------------------------
      radioButtons(inputId = "z",
                   label = "Color By: ",
                   choices = c("state","region"),
                   selected = "state"),
      
      
      # Select region
      checkboxGroupInput(inputId = "selected_area",
                         label = "Select region(s):",
                         choices = c("Midwest", "South", "Northeast","West","Other"),
                         selected = "South"),
      
      # Select sample size 
      numericInput(inputId = "n_samp", 
                   label = "Sample size:", 
                   min = 1, max = nrow(oil), 
                   value = 100),
      
      # Horizontal line for visual separation
      hr(),
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      downloadButton('downloadData', 'Download data'),
      
      hr(),
      actionButton("button","Print the frequency of states")
    ),
    
    # Output: Show plots
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      br(),
      plotOutput(outputId = "piechart"),
      br(),# a little bit of visual separation
      plotOutput(outputId = "boxplot"),
      br(),# a little bit of visual separation
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "oiltable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  # Create a subset of data filtering for selected region
  oil_subset <- reactive({
    req(input$selected_area) # ensure availablity of value before proceeding
    filter(oil, region %in% input$selected_area)
  })
  
  # Create new df that is n_samp obs from seoected region
  oil_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(oil_subset(), input$n_samp)
  })
  


  # bar plot
  output$scatterplot <- renderPlot({
    ggplot(data=oil_sample(),aes_string(x = input$x, y=input$y,fill=input$z)) +
      geom_bar(stat="identity")+labs(title = paste("Bar Chart: ",input$y," By ",input$x) )
    
  })
  
  #pie chart
  output$piechart <- renderPlot({
    ggplot(data = oil_sample(), aes_string(x = factor(1), y = input$y, fill=input$z)) + 
      geom_bar(stat="identity", width=1)+ coord_polar("y", start = 0) +
      labs(x = NULL, y = NULL, title = paste("Pie Chart: ",input$y ))
  })
  
  #box plot 
  output$boxplot <- renderPlot({
    ggplot(data = oil_sample(), aes_string(x = input$x, y=input$y, color=input$z)) + 
      geom_boxplot()+labs(title = paste("Boxplot: ",input$y," By ",input$x) )
  })
  
  
  
  # Print data table if checked -------------------------------------
  output$oiltable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = oil_sample(), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  # download data when button is clicked
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("oildatasample",gsub(":","-",Sys.time()), ".csv", sep="")
    },
    content = function(file) {
      write.csv(oil_sample(), file, row.names = FALSE)
    })
  
  #obsesrve the click button and print the result in the console
  observeEvent(input$button, {
    newVal <- count(oil_sample()$state)
    print("Frequency of the states")
    print(newVal)
    
  })
  

}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
