source("helperFunctions.R")
source("allDataGraphs.R")
source("communityGraphs.R")
source("companyGraphs.R")

# load libraries
library(lubridate)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(leaflet)
# library(leaflet.providers)
library(DT)
library(scales)
library(tibble)
library(tidyverse)
library(maptools)
library(rgdal)



communities <- getCommNames()   #list of comm names
areasDATA <- getAreas()         #df
companiesnames <-getCompNames() #list of comp names
companydata <-getComp()         #df


# data for plots
allmilesdata <- getallmileagedata()
allsecsdata <- getallsecsdata()
allyeardata <- readAllYearData()
allhourdata <- readAllHoursData()
allmonthdata <- readAllMonthData()
alldaydata <- readAllDayData()

# get the shape file for the map 
# cite where we got the shape file from https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
areas <- readOGR('shapefiles')
shp <- spTransform(areas, CRS("+proj=longlat +datum=WGS84"))


ui <- fluidPage( div(style = "height:3100px;font-size: 30px",
  
  # App title ----
  titlePanel("Project 3 - Chicago Taxi Rides in 2019"),
  # h1(id="heading", "Project 3 - Chicago Taxi Rides in 2019"),
  # tags$style(HTML("#heading{background-color: blue;
  #                           font-size: 50px;}")),

  
  splitLayout(cellWidths = c("30%","70%"),
              verticalLayout(div(style = "height:3100px;", leafletOutput("leaf", height="70%"),

                
                sidebarPanel(width = 12,div(style = "height:500px;",
                             
                             # # Input: Select the random distribution type ----
                             # radioButtons("Len", "Distance Conversion:",
                             #              c("Kilometers" = "kilo",
                             #                "Miles" = "miles")),
                             # # br() element to introduce extra vertical spacing ----
                             # radioButtons("time", "Time Conversion:",
                             #              c("24 Hour" = "24H",
                             #                "12 Hour" = "12H")),
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
              verticalLayout(
              navlistPanel(widths = c(1,10),
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
                tabPanel("Plots",fluidRow(splitLayout(cellWidths =c('33%','33%','33%'),plotOutput("yearplot1",height = "1200px"),plotOutput("monthplot1",height = "1200px"),plotOutput("dayplot1",height = "1200px"))), 
                         fluidRow(box(height = 150)),
                         fluidRow(splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("milesplot1",height = "1200px"),plotOutput("hourplot1",height = "1200px"),plotOutput("timeplot1",height = "1200px")))
                ),
                tabPanel("Tabular",fluidRow(box(height = 300)),
                         fluidRow(splitLayout(cellWidths =c('5%','25%','5%','35%','5%','35%'),box(),dataTableOutput("yeartable1"),box(),dataTableOutput("monthtable1"),box(),dataTableOutput("daytable1"))),
                         fluidRow(box(height = 300)),
                         fluidRow(splitLayout(cellWidths =c('5%','25%','5%','35%','5%','35%'),box(),dataTableOutput("milestable1"),box(),dataTableOutput("hourtable1"),box(),dataTableOutput("timetable1")))
                         ),
                "Community",
                tabPanel("Plots ",fluidRow(splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("yearplot2",height = "1200px"),plotOutput("monthplot2",height = "1200px"),plotOutput("dayplot2",height = "1200px"))),
                         fluidRow(box(height = 150)),         
                         fluidRow(splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("milesplot2",height = "1200px"),plotOutput("hourplot2",height = "1200px"),plotOutput("timeplot2",height = "1200px")))
                ),
                tabPanel("Tabular",fluidRow(box(height = 300)),
                         fluidRow(splitLayout(cellWidths =c('5%','25%','5%','35%','5%','35%'),box(),dataTableOutput("yeartable2"),box(),dataTableOutput("monthtable2"),box(),dataTableOutput("daytable2"))),
                         fluidRow(box(height = 300)),
                         fluidRow(splitLayout(cellWidths =c('5%','25%','5%','35%','5%','35%'),box(),dataTableOutput("milestable2"),box(),dataTableOutput("hourtable2"),box(),dataTableOutput("timetable2")))
                ),
                "Company",
                tabPanel("Plots ",fluidRow(splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("yearplot3",height = "1200px"),plotOutput("monthplot3",height = "1200px"),plotOutput("dayplot3",height = "1200px"))),
                         fluidRow(box(height = 150)),         
                         fluidRow(splitLayout(cellWidths = c('33%','33%','33%'),plotOutput("milesplot3",height = "1200px"),plotOutput("hourplot3",height = "1200px"),plotOutput("timeplot3",height = "1200px")))
                ),
                tabPanel("Tabular",fluidRow(box(height = 300)),
                         fluidRow(splitLayout(cellWidths =c('5%','25%','5%','35%','5%','35%'),box(),dataTableOutput("yeartable3"),box(),dataTableOutput("monthtable3"),box(),dataTableOutput("daytable3"))),
                         fluidRow(box(height = 300)),
                         fluidRow(splitLayout(cellWidths =c('5%','25%','5%','35%','5%','35%'),box(),dataTableOutput("milestable3"),box(),dataTableOutput("hourtable3"),box(),dataTableOutput("timetable3")))
                ),
                tabPanel("Map",verticalLayout(verticalLayout(div(style = "height:3100px;",leafletOutput("leaf2", height="50%"), sidebarPanel(width = 12,
                                                                                   
                                                                                   # Input: Select the company alphabetically  ----
                                                                                   selectInput("company2", "Select Company", companiesnames),
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
              ),
              fluidRow(splitLayout(cellWidths =c('10%','30%','30%','30%'),
                                   box(),
                # Input: Select the random distribution type ----
                       radioButtons("Len", "Distance Conversion:",
                                    c("Kilometers" = "kilo",
                                      "Miles" = "miles")),
                       # br() element to introduce extra vertical spacing ----
                       radioButtons("time", "Time Conversion:",
                                    c("24 Hour" = "24H",
                                      "12 Hour" = "12H")),
                        box()
                )
              )
              )
  )     
 )
)



server <- function(input, output) {
  
  # initial plot with all data
  
  output$yearplot1 <- renderPlot({yearplot1(allyeardata)})
  
  output$hourplot1 <-renderPlot({hourplot1(input$time,allhourdata)})

  output$monthplot1 <- renderPlot ({monthplot1(allmonthdata)})

  output$dayplot1 <- renderPlot ({dayplot1(alldaydata)})

  output$milesplot1 <- renderPlot ({milesplot1(input$Len, allmilesdata)})

  output$timeplot1 <-renderPlot ({secsplot1(allsecsdata)})

  
  # plots according to selected community area

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

  output$milesplot2 <- renderPlot ({milesplot2(getCommDataReactive(), input$community, input$toFrom, input$Len)}) 

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

  output$milesplot3 <- renderPlot ({milesplot3(getCompanyDataReactive(), input$company, input$Len)})

  output$timeplot3 <-renderPlot ({secsplot3(getCompanyDataReactive(), input$company)})
  
  # TABLES --
  
  output$yeartable1 <- DT::renderDataTable(
    DT::datatable({
      colnames(allyeardata) <- c('Date', 'Count')
      allyeardata$Count <- format(allyeardata$Count, big.mark = ",", scientific = FALSE)
      allyeardata},
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))), rownames = FALSE
  ))
  
  output$hourtable1 <- DT::renderDataTable(
    DT::datatable({
      colnames(allhourdata) <- c('Hour', 'Count')
      allhourdata$Count <- format(allhourdata$Count, big.mark = ",", scientific = FALSE)
      allhourdata},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))), rownames = FALSE
    ))
  
  output$monthtable1 <- DT::renderDataTable(
    DT::datatable({
      colnames(allmonthdata) <- c('Month', 'Count')
      allmonthdata$Count <- format(allmonthdata$Count, big.mark = ",", scientific = FALSE)
      allmonthdata},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))), rownames = FALSE
    ))

  output$daytable1 <- DT::renderDataTable(
    DT::datatable({
      colnames(alldaydata) <- c('Day', 'Count')
      alldaydata$Count <- format(alldaydata$Count, big.mark = ",", scientific = FALSE)
      alldaydata <- alldaydata[match(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), alldaydata$Day),]
      alldaydata},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE), rownames = FALSE
    ))
  
  output$milestable1 <- DT::renderDataTable(
    DT::datatable({
      breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
      tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
                "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
      group_tags <- cut(allmilesdata$miles,
                        breaks=breaks,
                        include.lowest=TRUE,
                        right=FALSE,
                        labels=tags)
      d<- data.frame(group_tags)
      d <-aggregate(d$group_tags, by=list(d$group_tags), FUN= length)
      colnames(d) <- c('Miles', 'Count')
      d$Count <- format(d$Count, big.mark = ",", scientific = FALSE)
      d
      },
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE), rownames = FALSE
    ))
  
  output$timetable1 <- DT::renderDataTable(
    DT::datatable({
      breaks <- c(0, 180, 300, 420, 480, 540, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 18001)
      tags <- c("3min","5min","7min","8min","9min","10min","12min","15min","20min","30min","45min","1hr","2hr","=>3hr")
      
      group_tags <- cut(allsecsdata$secs,
                        breaks=breaks,
                        include.lowest=TRUE,
                        right=FALSE,
                        labels=tags)
      d<- data.frame(group_tags)
      d <-aggregate(d$group_tags, by=list(d$group_tags), FUN= length)
      colnames(d) <- c('Time', 'Count')
      d$Count <- format(d$Count, big.mark = ",", scientific = FALSE)
      d},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE ), rownames = FALSE
    ))
  
  #####################################################################
  
  output$yeartable2 <- DT::renderDataTable(
    DT::datatable({
      d <- getCommDataReactive()
      data <- aggregate(d$day, by=list(d$date), FUN= length)
      colnames(data) <- c('Date', 'Count')
      data},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))), rownames = FALSE
    ))
  
  output$monthtable2 <- DT::renderDataTable(
    DT::datatable({
      d <- getCommDataReactive()
      data <- aggregate(d$day, by=list(d$month), FUN= length)
      colnames(data) <- c('Month', 'Count')
      data},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))), rownames = FALSE
    ))
  
  output$daytable2 <- DT::renderDataTable(
    DT::datatable({
      d <- getCommDataReactive()
      data <- aggregate(d$day, by=list(d$wday), FUN= length)
      colnames(data) <- c('Day', 'Count')
      daydata <- data[match(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), data$Day),]
      daydata},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE), rownames = FALSE
    ))
  
  output$hourtable2 <- DT::renderDataTable(
    DT::datatable({
      d <- getCommDataReactive()
      data <- aggregate(d$day, by=list(d$hour), FUN= length)
      colnames(data) <- c('Hour', 'Count')
      data},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))), rownames = FALSE
    ))
  
  output$milestable2 <- DT::renderDataTable(
    DT::datatable({
      d <- getCommDataReactive()
      
      breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
      tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
                "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
      group_tags <- cut(d$miles,
                        breaks=breaks,
                        include.lowest=TRUE,
                        right=FALSE,
                        labels=tags)
      d<- data.frame(group_tags)
      d <-aggregate(d$group_tags, by=list(d$group_tags), FUN= length)
      colnames(d) <- c('Miles', 'Count')
      d$Count <- format(d$Count, big.mark = ",", scientific = FALSE)
      d},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE), rownames = FALSE
      
    ))
  
  output$timetable2 <- DT::renderDataTable(
    DT::datatable({
      d <- getCommDataReactive()
      
      breaks <- c(0, 180, 300, 420, 480, 540, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 18001)
      tags <- c("3min","5min","7min","8min","9min","10min","12min","15min","20min","30min","45min","1hr","2hr","=>3hr")
      
      group_tags <- cut(d$secs,
                        breaks=breaks,
                        include.lowest=TRUE,
                        right=FALSE,
                        labels=tags)
      d<- data.frame(group_tags)
      d <-aggregate(d$group_tags, by=list(d$group_tags), FUN= length)
      colnames(d) <- c('Time', 'Count')
      d$Count <- format(d$Count, big.mark = ",", scientific = FALSE)
      d},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE), rownames = FALSE
      
    ))
  
  ############################################################
  
  output$yeartable3 <- DT::renderDataTable(
    DT::datatable({
      d <- getCompanyDataReactive()
      data <- aggregate(d$day, by=list(d$date), FUN= length)
      colnames(data) <- c('Date', 'Count')
      data},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))), rownames = FALSE
    ))
  
  output$monthtable3 <- DT::renderDataTable(
    DT::datatable({
      d <- getCompanyDataReactive()
      data <- aggregate(d$day, by=list(d$month), FUN= length)
      colnames(data) <- c('Month', 'Count')
      data},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))), rownames = FALSE
    ))
  
  output$daytable3 <- DT::renderDataTable(
    DT::datatable({
      d <- getCompanyDataReactive()
      data <- aggregate(d$day, by=list(d$wday), FUN= length)
      colnames(data) <- c('Day', 'Count')
      daydata <- data[match(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), data$Day),]
      daydata},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE), rownames = FALSE
    ))
  
  output$hourtable3 <- DT::renderDataTable(
    DT::datatable({
      d <- getCompanyDataReactive()
      data <- aggregate(d$day, by=list(d$hour), FUN= length)
      colnames(data) <- c('Hour', 'Count')
      data},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))), rownames = FALSE
    ))
  
  output$milestable3 <- DT::renderDataTable(
    DT::datatable({
      d <- getCompanyDataReactive()
      
      breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
      tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
                "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
      group_tags <- cut(d$miles,
                        breaks=breaks,
                        include.lowest=TRUE,
                        right=FALSE,
                        labels=tags)
      d<- data.frame(group_tags)
      d <-aggregate(d$group_tags, by=list(d$group_tags), FUN= length)
      colnames(d) <- c('Miles', 'Count')
      d$Count <- format(d$Count, big.mark = ",", scientific = FALSE)
      d},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE), rownames = FALSE
      
    ))
  
  output$timetable3 <- DT::renderDataTable(
    DT::datatable({
      d <- getCompanyDataReactive()
      
      breaks <- c(0, 180, 300, 420, 480, 540, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 18001)
      tags <- c("3min","5min","7min","8min","9min","10min","12min","15min","20min","30min","45min","1hr","2hr","=>3hr")
      
      group_tags <- cut(d$secs,
                        breaks=breaks,
                        include.lowest=TRUE,
                        right=FALSE,
                        labels=tags)
      d<- data.frame(group_tags)
      d <-aggregate(d$group_tags, by=list(d$group_tags), FUN= length)
      colnames(d) <- c('Time', 'Count')
      d$Count <- format(d$Count, big.mark = ",", scientific = FALSE)
      d},
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE ), rownames = FALSE
      
    ))
  
  #######################################################
  
  ## reactive functions for selected area 
  getAreaDROPReactive <- reactive({ getDATADrop(getCommIDReactive()) })
  getAreaPICKReactive <- reactive({ getDATAPick(getCommIDReactive()) })
  
  ## leaflet map
  output$leaf <- renderLeaflet({
    
    if(input$toFrom == 'pickup' ){
      # print(input$community)
      # print("start radio")
      DATAarea <-getAreaPICKReactive()
      
      tripByArea <- DATAarea %>%
        select(`pickup`,`dropoff`) %>%
        gather(variable,area_num_1) %>%
        count(variable,area_num_1) %>%
        drop_na(area_num_1) %>%
        mutate(area_num_1 = as.character(area_num_1))
      
      total <-tripByArea$n[length(tripByArea$n)] 
      # print(total)
      tripByArea$percentage <- ((tripByArea$n)/total)*100
      
      
    }else if(input$toFrom == 'dropoff' ){
      # print(input$community)
      # print("end radio")
      DATAarea <-getAreaDROPReactive()
      
      tripByArea <- DATAarea %>%
        select(`pickup`,`dropoff`) %>%
        gather(variable,area_num_1) %>%
        count(variable,area_num_1) %>%
        drop_na(area_num_1) %>%
        mutate(area_num_1 = as.character(area_num_1))
      
      total <-tripByArea$n[1] 
      # print(total)
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
                  layerId = ~shpPickUp@data$community,
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
        position="bottomright",
        options = layersControlOptions(collapsed = FALSE)
      ) 
    
  })
  
  #######################################################
  
  ## reactive functions for selected area
  getIDCOMPReactive <- reactive({ companydata[companydata$company==input$company2, "id"] })
  getID2Reactive <- reactive({ as.character(areasDATA[areasDATA$community == input$area, "area_id"]) })

  getCOMPDROPReactive <- reactive({ getDATA2Drop(getID2Reactive(),getIDCOMPReactive()) })
  getCOMPPICKReactive <- reactive({ getDATA2Pick(getID2Reactive(),getIDCOMPReactive()) })

  ## leaflet map
  output$leaf2 <- renderLeaflet({

    if(input$toFrom2 == 'start' ){
      # print(input$area)
      # print(input$company)

      # print("start radio")
      DATAarea2 <-getCOMPPICKReactive()

      tripByArea2 <- DATAarea2 %>%
        select(`pickup`,`dropoff`) %>%
        gather(variable,area_num_1) %>%
        count(variable,area_num_1) %>%
        drop_na(area_num_1) %>%
        mutate(area_num_1 = as.character(area_num_1))

      total <-tripByArea2$n[length(tripByArea2$n)]
      # print(total)
      tripByArea2$percentage <- ((tripByArea2$n)/total)*100


    }else if(input$toFrom2 == 'end' ){
      # print(input$area)
      # print(input$company)
      # print("end radio")
      DATAarea2 <-getCOMPDROPReactive()

      tripByArea2 <- DATAarea2 %>%
        select(`pickup`,`dropoff`) %>%
        gather(variable,area_num_1) %>%
        count(variable,area_num_1) %>%
        drop_na(area_num_1) %>%
        mutate(area_num_1 = as.character(area_num_1))

      total <-tripByArea2$n[1]
      # print(total)
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
        position ="bottomright"
      )

  })
  
  #############################
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  getIDClickReactive <- reactive( {as.character(areasDATA[areasDATA$community == data_of_click$clickedMarker, "area_id"]) })
  getAreaDROPReactiveClick <- reactive({ getDATADrop(getIDClickReactive()) })
  getAreaPICKReactiveClick <- reactive({ getDATAPick(getIDClickReactive()) })
  
  observeEvent(input$leaf_shape_click, {
    #create object for clicked polygon
    click <- input$leaf_shape_click
    
    print(click$id)
    data_of_click$clickedMarker <- click$id
    
    print(data_of_click$clickedMarker)
    
    #DATAarea <-getIDClickReactive()
    
    # print(DATAarea)
    
    if(input$toFrom == 'pickup' ){
      print(input$community)
      print("start radio")
      DATAarea <-getAreaPICKReactiveClick()
      
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
      DATAarea <-getAreaDROPReactiveClick()
      
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
    
    leafletProxy("leaf", data = shpPickUp) %>% 
      addTiles() %>% 
      setView(lat=41.891105, lng=-87.652480,zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=shpPickUp,
                  weight=1,
                  layerId = ~shpPickUp@data$community,
                  fillColor = ~pal(percentage),
                  fillOpacity = 0.6,
                  group = "Pick-Ups",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label=~theLabelsPickUp) %>%
      addPolygons(data=shpDropOff,
                  weight=1,
                  # layerId = ~shpDropOff@data$community,
                  fillColor = ~pal(percentage),
                  fillOpacity = 0.6,
                  group = "Drop-offs",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label=~theLabelsDropOff) %>%
      addLayersControl(
        baseGroups = c("Pick-Ups", "Drop-offs"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      )
  })
  
}


shinyApp(ui = ui, server = server)
