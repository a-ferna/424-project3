
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
library(tibble)
library(tidyverse)
library(maptools)
library(rgdal)
library(viridis)


# get the shape file for the map 
# cite where we got the shape file from https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
areas <- readOGR('shapefiles')
shp <- spTransform(areas, CRS("+proj=longlat +datum=WGS84"))


alldata <- getAllData()

#placeholder for communities
communities <- getCommNames()
areasDATA <- getAreas()

companiesnames <-getCompNames()
companydata <-getComp()

# Define UI for application that draws a histogram
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Project 3 - Taxi Trips"),

  # Sidebar layout with input and output definitions ----
  #sidebarLayout(
  splitLayout(cellWidths = c("30%","70%"),
      verticalLayout(        
      leafletOutput("leaf"),
    
      sidebarPanel(width = 12,
                   
                   # Input: Select the random distribution type ----
                   radioButtons("Len", "Distance Conversion:",
                                c("Kilometers" = "kilo",
                                  "Miles" = "miles")),
                   # br() element to introduce extra vertical spacing ----
                   radioButtons("time", "Time Conversion:",
                                c("24 Hour" = "24H",
                                  "12 Hour" = "12H")),
                   # Input: Select the community alphabetically  ----
                   selectInput("community", "Select Community", communities),
                   # br() element to introduce extra vertical spacing ----
                   radioButtons("toFrom", "From", 
                                c("start" = "pickup",
                                  "end" = "dropoff"))
      )),
        # Main panel for displaying outputs ----

          # Output: Tabset w/ plot, summary, and table ----
          navlistPanel(
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            "All Data",
            tabPanel("Plots",fluidRow( splitLayout(cellWidths = c('50%','50%'),plotOutput("plot"),plotOutput("plot2"))), 
                              fluidRow(splitLayout(cellWidths = c('50%','50%'),plotOutput("plot3"),plotOutput("plot4")))
            ),
            tabPanel("Tabular",plotOutput("plotTab")),
            "Community",
            tabPanel("Plots ",fluidRow( splitLayout(cellWidths = c('50%','50%'),plotOutput("p5"),plotOutput("p6")))),
            tabPanel("Tabular",plotOutput("t1")),
            "Company",
            tabPanel("Plots ",fluidRow( splitLayout(cellWidths = c('50%','50%'),plotOutput("p7"),plotOutput("p8")))),
            tabPanel("Tabular",plotOutput("tab3")),
            tabPanel("Map",verticalLayout(leafletOutput("leaf2"), sidebarPanel(width = 12,
                                                                              
                                                                               # Input: Select the company alphabetically  ----
                                                                               selectInput("company", "Select Company", companiesnames),
                                                                               selectInput("area", "Select Community", communities),
                                                                  
                                                                               # br() element to introduce extra vertical spacing ----
                                                                               radioButtons("toFrom2", "From", 
                                                                                            c("start" = "start",
                                                                                              "end" = "end"))
            ))),
            tabPanel("About",p("Project 3 - Big Yellow Taxi"),p("Data file from the Chicago Data Portal"),
                     p("An interactive visualization in R and Shiny on Shinyapps.io"),
                     p("Dashboard initially shows Map and a tabset of plots"),
                     
                     p("Written by Ariadna Fernandez and Andrea Herrera ") ), selected = "About"
          )
  
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Generate a plot of the data ----

  output$plot <- renderPlot({
    dates <- data.frame(alldata$date)
    colnames(dates) <- c("date")
    
    ggplot(dates, aes(x=date)) +
      geom_histogram(binwidth=.5) +
      geom_density(alpha=.2, fill="red")  #density doesn't show
  })
  
  
  output$plot2 <- renderPlot({
    ggplot(alldata, aes(x=hour, fill=..x..)) +
      geom_bar(stat="count") +
      labs(x="Hour", y="Total Rides", title="Total Rides by Hour of the Day") +
      scale_fill_gradientn(labels=NULL, colors=c("orangered2", "yellow", "blue2"))+
      theme(legend.position = "none") +
      scale_y_continuous(labels = comma, breaks = seq(0, 900000, 100000)) +
      # scale_x_continuous(breaks = seq(0, 23, 1)) 
      scale_x_continuous(breaks = seq(0, 23, 1),
                         labels =c("12am","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm"),
                         guide = guide_axis(angle = 40))
  })
  
  output$plot3 <- renderPlot({
    ggplot(alldata, aes(x=wday)) +
      geom_bar(stat="count", fill="skyblue3") +
      scale_y_continuous(labels = comma, breaks = seq(0, 2000000, 250000)) +
      scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
      labs(x="Day of the Week", y="Total Rides", title="Total Rides by Day of the Week")
  })
  
  output$plot4 <- renderPlot({
    ggplot(alldata, aes(x=month)) +
      geom_bar(stat="count", fill="palegreen3") +
      scale_y_continuous(labels = comma, breaks = seq(0, 1200000, 200000)) +
      scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      labs(x="Month", y="Total Rides", title="Total Rides by Month")
  })
  
  
  output$p5 <-renderPlot({
    # dist number of rides by binned mileage
    breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
    tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
              "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
    group_tags <- cut(alldata$miles, 
                      breaks=breaks, 
                      include.lowest=TRUE, 
                      right=FALSE, 
                      labels=tags)
     summary(group_tags)
    
    ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
      geom_bar(width = 0.9) +
      scale_fill_gradientn(colours = c("plum1","mediumpurple3","darkorchid3")) +
      labs(x="Miles", y="Total Rides", title="Number of Rides by Mileage") +
      scale_y_continuous(labels = comma) +
      # scale_x_discrete(guide = guide_axis(angle = 10)) +
      theme(legend.position = "none") 
    })
  
  
  output$p6 <-renderPlot({
    breaks <- c(0, 180, 300, 420, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 10800, 14400, 18000)
    tags <- c("3min","5min","7min","10min","12min","15min","20min","30min","45min","1hr","2hr","3hr","4hr","5hr")
    # breaks <- c(0, 100, 200, 300, 400, 500, 600, 750, 1000, 1500, 2000, 3000, 5000, 10000, 15000, 18000)
    # tags <- c("60-100","100-200","200-300","300-400","400-500","500-600","600-750","750-1000","1000-1500","1500-2000","2000-3000", "3000-5000", "5000-10000", "10000-15000", "15-18000")
    
    group_tags <- cut(alldata$secs,
                      breaks=breaks,
                      include.lowest=TRUE,
                      right=FALSE,
                      labels=tags)
    
    ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
      geom_bar() +
      labs(x="Trip time", y="Count", title="Number of Rides by Trip Time") +
      scale_y_continuous(labels = comma) +
      theme(legend.position = "none")
  })
  
  
  output$p7 <-renderPlot({

  })
  
  
  ## reactive functions for selected area 
  getIDReactive <- reactive({ as.character(areasDATA[areasDATA$community == input$community, "area_id"]) })
  getAreaDROPReactive <- reactive({ getDATADrop(getIDReactive()) })
  getAreaPICKReactive <- reactive({ getDATAPick(getIDReactive()) })
  
  
  ## leaflet map
  output$leaf <- renderLeaflet({
    
    if(input$toFrom == 'pickup' ){
      print(input$community)
      print("start radio")
      DATAarea <-getAreaPICKReactive()
      
      tripByArea <- DATAarea %>%
        select(`pickup`,`dropoff`) %>%
        gather(variable,area_num_1) %>%
        count(variable,area_num_1) %>%
        drop_na(area_num_1) %>%
        mutate(area_num_1 = as.character(area_num_1))
      
      total <-tripByArea$n[length(tripByArea$n)] 
      print(total)
      tripByArea$percentage <- ((tripByArea$n)/total)*100
      
      
    }else if(input$toFrom == 'dropoff' ){
      print(input$community)
      print("end radio")
      DATAarea <-getAreaDROPReactive()
      
      tripByArea <- DATAarea %>%
        select(`pickup`,`dropoff`) %>%
        gather(variable,area_num_1) %>%
        count(variable,area_num_1) %>%
        drop_na(area_num_1) %>%
        mutate(area_num_1 = as.character(area_num_1))
      
      total <-tripByArea$n[1] 
      print(total)
      tripByArea$percentage <- ((tripByArea$n)/total)*100
      
      
    }
    
    #areaTest 
    DATAarea <-getDATADrop(40)
    #colorQuantile()
    
    
    leaflet(shp) %>% 
      addTiles() %>% 
      setView(lat=41.891105, lng=-87.652480,zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=shp,
                  weight=1,
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE)
      )
    
    
   
    shpPickUp <- shp
    shpDropOff <- shp
    
    shpPickUp@data <- shpPickUp@data %>%
      left_join(filter(tripByArea,variable == 'pickup'), 
                by = 'area_num_1')
    
    shpDropOff@data <- shpDropOff@data %>%
      left_join(filter(tripByArea,variable == 'dropoff'), 
                by = 'area_num_1')
    
    bins <- c(0, 0.3, 0.5, 1, 2,3,4,5,6,7,9,10,50,100)
    pal <- colorBin("inferno", domain = (shp@data$percentage), bins = bins)
    
    theLabelsPickUp <- sprintf(
      "<strong>Community: %s</strong><br/>percentage=%g",
      shpPickUp@data$community, shpPickUp@data$percentage
    ) %>% lapply(htmltools::HTML)
    
    theLabelsDropOff <- sprintf(
      "<strong>Community: %s</strong><br/>percentage=%g",
      shpDropOff@data$community, shpDropOff@data$percentage
    ) %>% lapply(htmltools::HTML)
    
    leaflet(shpPickUp) %>% 
      addTiles() %>% 
      setView(lat=41.891105, lng=-87.652480,zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=shpPickUp,
                  weight=1,
                  fillColor = ~pal(percentage),
                  fillOpacity = 0.6,
                  group = "Pick-Ups",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label=~theLabelsPickUp) %>%
      addPolygons(data=shpDropOff,
                  weight=1,
                  fillColor = ~pal(percentage),
                  fillOpacity = 0.6,
                  group = "Drop-offs",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label=~theLabelsDropOff) %>%
      addLegend(pal = pal, 
                values = ~percentage,
                opacity = 0.6, 
                title = "Taxi Trips By Area %",
                position = "topright") %>%
      addLayersControl(
        baseGroups = c("Pick-Ups", "Drop-offs"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
   
  })
  
  ## reactive functions for selected area 
  getIDCOMPReactive <- reactive({ companydata[companydata$company==input$company, "id"] })
  getID2Reactive <- reactive({ as.character(areasDATA[areasDATA$community == input$area, "area_id"]) })
  
  getCOMPDROPReactive <- reactive({ getDATA2Drop(getID2Reactive(),getIDCOMPReactive()) })
  getCOMPPICKReactive <- reactive({ getDATA2Pick(getID2Reactive(),getIDCOMPReactive()) })
  
  
  ## leaflet map
  output$leaf2 <- renderLeaflet({
    
    if(input$toFrom2 == 'start' ){
      print(input$area)
      print(input$company)
      
      print("start radio")
      DATAarea2 <-getCOMPPICKReactive()
      
      tripByArea2 <- DATAarea2 %>%
        select(`pickup`,`dropoff`) %>%
        gather(variable,area_num_1) %>%
        count(variable,area_num_1) %>%
        drop_na(area_num_1) %>%
        mutate(area_num_1 = as.character(area_num_1))
      
      total <-tripByArea2$n[length(tripByArea2$n)] 
      print(total)
      tripByArea2$percentage <- ((tripByArea2$n)/total)*100
      
      
    }else if(input$toFrom2 == 'end' ){
      print(input$area)
      print(input$company)
      print("end radio")
      DATAarea2 <-getCOMPDROPReactive()
      
      tripByArea2 <- DATAarea2 %>%
        select(`pickup`,`dropoff`) %>%
        gather(variable,area_num_1) %>%
        count(variable,area_num_1) %>%
        drop_na(area_num_1) %>%
        mutate(area_num_1 = as.character(area_num_1))
      
      total <-tripByArea2$n[1] 
      print(total)
      tripByArea2$percentage <- ((tripByArea2$n)/total)*100
      
      
    }
    
    
    leaflet(shp) %>% 
      addTiles() %>% 
      setView(lat=41.891105, lng=-87.652480,zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=shp,
                  weight=1,
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE)
      )
    
    
    
    shpPickUp <- shp
    shpDropOff <- shp
    
    shpPickUp@data <- shpPickUp@data %>%
      left_join(filter(tripByArea2,variable == 'pickup'), 
                by = 'area_num_1')
    
    shpDropOff@data <- shpDropOff@data %>%
      left_join(filter(tripByArea2,variable == 'dropoff'), 
                by = 'area_num_1')
    
    bins <- c(0, 0.3, 0.5, 1, 2,3,4,5,6,7,9,10,50,100)
    pal <- colorBin("inferno", domain = (shp@data$percentage), bins = bins)
    
    theLabelsPickUp <- sprintf(
      "<strong>Community: %s</strong><br/>percentage=%g",
      shpPickUp@data$community, shpPickUp@data$percentage
    ) %>% lapply(htmltools::HTML)
    
    theLabelsDropOff <- sprintf(
      "<strong>Community: %s</strong><br/>percentage=%g",
      shpDropOff@data$community, shpDropOff@data$percentage
    ) %>% lapply(htmltools::HTML)
    
    leaflet(shpPickUp) %>% 
      addTiles() %>% 
      setView(lat=41.891105, lng=-87.652480,zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=shpPickUp,
                  weight=1,
                  fillColor = ~pal(percentage),
                  fillOpacity = 0.6,
                  group = "Pick-Ups",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label=~theLabelsPickUp) %>%
      addPolygons(data=shpDropOff,
                  weight=1,
                  fillColor = ~pal(percentage),
                  fillOpacity = 0.6,
                  group = "Drop-offs",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label=~theLabelsDropOff) %>%
      addLegend(pal = pal, 
                values = ~percentage,
                opacity = 0.6, 
                title = "Taxi Trips By Company %",
                position = "topright") %>%
      addLayersControl(
        baseGroups = c("Pick-Ups", "Drop-offs"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


