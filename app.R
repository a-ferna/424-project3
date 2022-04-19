
source("data.R")

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



alldata <- getAllData()

#placeholder for communities
communities <- getComm()

# Define UI for application that draws a histogram
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Project 3"),

  # Sidebar layout with input and output definitions ----
  #sidebarLayout(
  fluidRow(column(width = 12,leafletOutput("leaf"))),
    
  sidebarPanel(width = '2',
               
               # Input: Select the random distribution type ----
               radioButtons("Len", "Distance Conversion:",
                            c("Kilometers" = "kilo",
                              "Miles" = "miles")),
               # br() element to introduce extra vertical spacing ----
               radioButtons("time", "Time Conversion:",
                            c("24 Hour" = "24H",
                              "12 Hour" = "12H"))
  ),
    # Main panel for displaying outputs ----
    mainPanel(
     
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(

        tabPanel("All Data",
                 navlistPanel(widths = c(2,10), tabPanel("Plot 1",fluidRow( splitLayout(cellWidths = c('25%','25%','25%','25%'),plotOutput("plot"),plotOutput("plot2"),plotOutput("plot3"),plotOutput("plot4")))),
                                                tabPanel("Plot 2",fluidRow( splitLayout(cellWidths = c('50%','50%'),plotOutput("p5"),plotOutput("p6")))),
                                                tabPanel("Tabular",plotOutput("plotTab")),
                              )
        ),
        tabPanel("Community",
                 navlistPanel(widths = c(2,10),  tabPanel("HeatMap ",plotOutput("p7")),
                              tabPanel("Percentage ",plotOutput("p8")),
                              tabPanel("Plots ",fluidRow( splitLayout(cellWidths = c('50%','50%'),plotOutput("p9"),plotOutput("p10")))),
                              tabPanel("Tabular",plotOutput("tab2"))),
        ),
        tabPanel("Company",
                 navlistPanel(widths = c(2,10), tabPanel("Plot ",fluidRow( splitLayout(cellWidths = c('50%','50%'),plotOutput("p11"),plotOutput("p12")))),
                              tabPanel("Tabular",plotOutput("tab3")))
        ),
        tabPanel("About",p("Project 3 - Big Yellow Taxi"),p("Data file from the Chicago Data Portal"),
                 p("An interactive visualization in R and Shiny on Shinyapps.io"),
                 p("Dashboard initially shows Map and a tabset of plots"),
                 
                 p("Written by Ariadna Fernandez and Andrea Herrera ") )
        
        
     # )
      
    )

  ),
  sidebarPanel(width = '2',
               
               # Input: Select the community alphabetically  ----
               selectInput("community", "Select Community", communities),
               # br() element to introduce extra vertical spacing ----
               radioButtons("toFrom", "to/from",
                            c("starting" = "start",
                              "ending" = "end"))
  ) 
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    #dist <- input$dist
    #n <- input$n
    
    dates <- data.frame(alldata$date)
    colnames(dates) <- c("date")
    
    ggplot(dates, aes(x=date)) +
      geom_histogram(binwidth=.5) +
      geom_density(alpha=.2, fill="#FF6666")

    # hist(d(),
    #      main = paste("r", dist, "(", n, ")", sep = ""),
    #      col = "#75AADB", border = "white")
  })
  
  output$plotTab <- DT::renderDataTable(
    DT::datatable({
      
      dates <- data.frame(alldata$date)
      colnames(dates) <- c("date")      
      
      dates$r <- format(dates$r, big.mark = ",", scientific = FALSE) 
      colnames(dates) <- c("Year", "Number of Riders")
      
      dates<-as.data.frame(dates)
      
    },
    
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE
    )
  )
  
  
  output$plot2 <- renderPlot({
    dates <- data.frame(alldata$date)
    colnames(dates) <- c("date")
    
    ggplot(dates, aes(x=date)) +
      geom_histogram(binwidth=.5) +
      geom_density(alpha=.2, fill="#FF6666")
  })
  
  output$plot3 <- renderPlot({
    dates <- data.frame(alldata$date)
    colnames(dates) <- c("date")
    
    ggplot(dates, aes(x=date)) +
      geom_histogram(binwidth=.5) +
      geom_density(alpha=.2, fill="#FF6666")
  })
  
  output$plot4 <- renderPlot({
    dates <- data.frame(alldata$date)
    colnames(dates) <- c("date")
    
    ggplot(dates, aes(x=date)) +
      geom_histogram(binwidth=.5) +
      geom_density(alpha=.2, fill="#FF6666")
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


