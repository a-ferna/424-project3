source("allDataGraphs.R")
source("data.R")
source("communityGraphs.R")
source("companyGraphs.R")

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

# needed variables
communities <- getCommNames() #list of comm names
areasDATA <- getAreas() #df with ids
companiesnames <-getCompNames() #list
companydata <-getComp() ##df 




# Define UI for application that draws a histogram
# Define UI for random distribution app ----
ui <- fluidPage(  div(style = "height:3240px;font-size: 70px",
  
  # App title ----
  titlePanel("Project 3 - Chicago Taxi Rides in 2019"),


  splitLayout(cellWidths = c("30%","70%"),
              verticalLayout(div(style = "height:3240px;", leafletOutput("leaf", height="50%"), sidebarPanel(width = 12,div(style = "height:1620px;",
                   
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
                                  "end" = "dropoff")),
                   selectInput("company", "Select Company",companiesnames)
      )))),
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
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            tabPanel(""),
            "All Data",
            tabPanel("Plots",fluidRow(div(style = "height:1600px;",splitLayout(cellWidths =c('33%','33%','33%'),plotOutput("yearplot1",height = "1620px"),plotOutput("monthplot1",height = "1620px"),plotOutput("dayplot1",height = "1620px")))), 
                             fluidRow(div(style = "height:1600px;",splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("milesplot1",height = "1620px"),plotOutput("hourplot1",height = "1620px"),plotOutput("timeplot1",height = "1620px"))))
            ),
            tabPanel("Tabular",plotOutput("plotTab")),
            "Community",
            tabPanel("Plots ",fluidRow(div(style = "height:1600px;",splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("yearplot2",height = "1620px"),plotOutput("monthplot2",height = "1620px"),plotOutput("dayplot2",height = "1620px")))),
                              fluidRow(div(style = "height:1600px;",splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("milesplot2",height = "1620px"),plotOutput("hourplot2",height = "1620px"),plotOutput("timeplot2",height = "1620px"))))
            ),
            tabPanel("Tabular",plotOutput("t1")),
            "Company",
            tabPanel("Plots ",fluidRow(div(style = "height:1600px;",splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("yearplot3",height = "1620px"),plotOutput("monthplot3",height = "1620px"),plotOutput("dayplot3",height = "1620px")))),
                              fluidRow(div(style = "height:1600px;",splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("milesplot3",height = "1620px"),plotOutput("hourplot3",height = "1620px"),plotOutput("timeplot3",height = "1620px"))))
            ),
            tabPanel("Tabular",plotOutput("tab3")),
            tabPanel("Map",verticalLayout(div(style = "height:3240px;",leafletOutput("leaf2", height="50%"), sidebarPanel(width = 12,div(style = "height:1620px;",
                                                                              
                                                                               # Input: Select the company alphabetically  ----
                                                                               selectInput("company", "Select Company", companiesnames),
                                                                               selectInput("area", "Select Community", communities),
                                                                  
                                                                               # br() element to introduce extra vertical spacing ----
                                                                               radioButtons("toFrom2", "From", 
                                                                                            c("start" = "start",
                                                                                              "end" = "end"))
            ))))),
            tabPanel("About",p("Project 3 - Big Yellow Taxi"),p("Data file from the Chicago Data Portal"),
                     p("An interactive visualization in R and Shiny on Shinyapps.io"),
                     p("Dashboard initially shows a Map, a set of Controls and a Nav set of plots\n"),
                     p("the first section contains plots of all the data,"),
                     p("THe then their is a section for the Community data and Company area all with plots"),
                     p("of the following described charts\n"),
                     p("bar chart showing the distribution of the number of rides by day of year (Jan 1 through Dec 31)\n"),
                     p("bar chart showing the distribution of the number of rides by hour of day based on start time (midnight through 11pm)\n"),
                     p("bar chart showing the distribution of the number of rides by day of week (Monday through Sunday)\n"),
                     p("bar chart showing the distribution of the number of rides by month of year (Jan through Dec)\n"),
                     p("bar chart showing the distribution of the number of rides by binned mileage (with an appropriate number of bins)\n"),
                     p("bar chart showing the distribution of the number of rides by binned trip time (with an appropriate number of bins)\n\n"),
                     p("the charts are controled by the radio buttons uner the map.\n\n"),
                     p("then all the same plots in tabular form, then"),
                     p("Finally the last tab opens up a seond map that shows the percentage of rides given to / from each community area by that taxi company."),
                     
                     p("shape files for the maps were downloaded at"),
                     p("https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6"),
                     p("app code modled after code from the following tutorial:"),
                     p("https://bar.rady.ucsd.edu/interactive_maps.html"),
                     p("Written by Ariadna Fernandez and Andrea Herrera ") ), selected = "About"
          )
  
  )
 )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  
  # Generate a plots of the data ----
  
  # initial plot with all data
  
  output$yearplot1 <- renderPlot({yearplot1()})
  
  output$hourplot1 <-renderPlot({hourplot1(input$time)})
  
  output$monthplot1 <- renderPlot ({monthplot1()})
  
  output$dayplot1 <- renderPlot ({dayplot1()})
  
  output$milesplot1 <- renderPlot ({milesplot1(input$Len)})
  
  output$timeplot1 <-renderPlot ({secsplot1()})
  
  
  ## plots according to selected community area
  
  # get community id corresponding to the selected name of the community area
  getCommIDReactive <- reactive ({areasDATA[areasDATA$community==input$community, "area_id"]})
  
  getCommDataReactive <- reactive ({getCommData(getCommIDReactive(), input$toFrom )})

  output$yearplot2 <- renderPlot({

    name <- str_to_title(input$community)
    data <- getCommDataReactive()
    yearplot2(input$community, data)
  })
  
  output$monthplot2 <- renderPlot ({monthplot2(getCommDataReactive(), input$community, input$toFrom)})

  output$dayplot2 <- renderPlot ({dayplot2(getCommDataReactive(), input$community, input$toFrom)})

  output$hourplot2 <-renderPlot ({hourplot2(getCommDataReactive(),input$time, input$community, input$toFrom)})

  output$milesplot2 <- renderPlot ({milesplot2(getCommDataReactive(), input$community, input$toFrom,input$Len)}) #kilometers!!!!

  output$timeplot2 <-renderPlot ({secsplot2(getCommDataReactive(), input$community, input$toFrom)})

  
  
  ## plots according to taxi companies
  
  getCompanyIDReactive <- reactive({companydata[companydata$company==input$company, "id"]})
  
  getCompanyDataReactive <- reactive ({getCompanyData(getCompanyIDReactive())})
  
  output$yearplot3 <- renderPlot({
    
    data <- getCompanyDataReactive()
    yearplot3(data, input$company)
  })
  
  output$monthplot3 <- renderPlot ({monthplot3(getCompanyDataReactive(), input$company)})
  
  output$dayplot3 <- renderPlot ({dayplot3(getCompanyDataReactive(), input$company)})
  
  output$hourplot3 <-renderPlot ({hourplot3(getCompanyDataReactive(),input$time, input$company)})
  
  output$milesplot3 <- renderPlot ({milesplot3(getCompanyDataReactive(), input$company, input$Len)}) #kilometers!!!!
  
  output$timeplot3 <-renderPlot ({secsplot3(getCompanyDataReactive(), input$company)})
 
  
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
    #DATAarea <-getDATADrop(40)
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
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
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
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      ) 
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


