

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# load libraries
library(lubridate)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.providers)
library(DT)
library(scales)

# Define UI for application that draws a histogram
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Project 3"),

  # Sidebar layout with input and output definitions ----
  #sidebarLayout(
  fluidRow(column(width = 12,leafletOutput("leaf"))),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      navlistPanel(
        tabPanel("Plots",plotOutput("plot2"),
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   
                   # Input: Select the random distribution type ----
                   radioButtons("dist", "Distribution type:",
                                c("Normal" = "norm",
                                  "Uniform" = "unif",
                                  "Log-normal" = "lnorm",
                                  "Exponential" = "exp")),
                   
                   # br() element to introduce extra vertical spacing ----
                   br(),
                   
                   # Input: Slider for the number of observations to generate ----
                   sliderInput("n",
                               "Number of observations:",
                               value = 500,
                               min = 1,
                               max = 1000)
                   
                 )   
                 
                 
          ),
        tabPanel("Summary", verbatimTextOutput("summary")
                 
        )
        
        
     # )
      
    )
    

  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#75AADB", border = "white")
  })
  
  
  output$plot2 <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#75AADB", border = "white")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d()
  })
  
  
  ## leaflet map
  output$leaf <- renderLeaflet({
    
    #dayData <- getDayDataReactive()
    
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -87.624347 , lat = 41.875672 , zoom = 15)
  
    
    # map <- addProviderTiles(map, "Hydda.RoadsAndLabels")
    # map <- addProviderTiles(map, "Stamen.TonerLite")
    # map <- addProviderTiles(map, "Esri.WorldImagery")
    map <- addProviderTiles(map, t)
   
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


