
library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)

oil <- read.csv("oildata.csv",header=TRUE, check.names = FALSE)


# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("state","rate_class", "num_oil_wells", "oil_prod_BBL","oil_wells_dayson"), 
                  selected = "oil_prod_BBL"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("state","rate_class", "num_oil_wells", "oil_prod_BBL","oil_wells_dayson"), 
                  selected = "num_oil_wells"),
      # Select variable for color -----------------------------------
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("state","rate_class", "num_oil_wells", "oil_prod_BBL","oil_wells_dayson"),
                  selected = "state"),
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Select which types of movies to plot ------------------------
      checkboxGroupInput(inputId = "selected_type",
                         label = "Select year(s):",
                         choices = c("2006", "2007", "2008","2009"),
                         selected = "2006"),
      
      # Select sample size ----------------------------------------------------
      numericInput(inputId = "n_samp", 
                   label = "Sample size:", 
                   min = 1, max = nrow(oil), 
                   value = 50)
    ),
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      br(),        # a little bit of visual separation
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "moviestable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  # Create a subset of data filtering for selected title types ------
  movies_subset <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(oil, prod_year %in% input$selected_type)
  })
  
  # Create new df that is n_samp obs from selected type movies ------
  movies_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(movies_subset(), input$n_samp)
  })
  
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = oil, aes_string(x = input$x, y = input$y,color = input$z)) +
      geom_point()
  })
  
  
  # Print data table if checked -------------------------------------
  output$moviestable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = movies_sample()[, 1:7], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  

}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

