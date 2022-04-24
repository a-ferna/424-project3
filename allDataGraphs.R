
# read data for each month and combine into one data frame
getAllData <- function() {
  
  jan <- read.table("month_files/month1.tsv", header=TRUE, sep=",")
  feb <- read.table("month_files/month2.tsv", header=TRUE, sep=",")
  mar <- read.table("month_files/month3.tsv", header=TRUE, sep=",")
  apr <- read.table("month_files/month4.tsv", header=TRUE, sep=",")
  may <- read.table("month_files/month5.tsv", header=TRUE, sep=",")
  jun <- read.table("month_files/month6.tsv", header=TRUE, sep=",")
  jul <- read.table("month_files/month7.tsv", header=TRUE, sep=",")
  aug <- read.table("month_files/month8.tsv", header=TRUE, sep=",")
  sep <- read.table("month_files/month9.tsv", header=TRUE, sep=",")
  oct <- read.table("month_files/month10.tsv", header=TRUE, sep=",")
  nov <- read.table("month_files/month11.tsv", header=TRUE, sep=",")
  dec <- read.table("month_files/month12.tsv", header=TRUE, sep=",")
  
  alldata <- rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
  
  alldata$date <- as.Date(with(alldata, paste("2019", month, day, sep="-")), "%Y-%m-%d")
  
  return (alldata)
}


# plot the total number of rides by day of the year
yearplot1 <- function(){
  
  # datesdata <- read.table("onlyDates.tsv", header = TRUE)
  # datesdata$date <- ymd(datesdata$date)
  
  datesdata <- read.table("DATES.csv", header = TRUE, sep=",")
  datesdata$date <- ymd(datesdata$date)
  
  ggplot(datesdata, aes(x=date, y=count)) +
    geom_bar(stat="identity") +
    # scale_x_date(date_labels = "%b")+
    labs(x='Date', y='Count', title="Distribution of taxi rides in 2019")+
    scale_y_continuous(labels = comma)

}



# plot the distribution of rides by hour
hourplot1 <- function(mode) {
  
  hourdata <- read.table("HOURS.csv", header = TRUE,sep=",")
  
  if(mode=="24H") {
    ggplot(hourdata, aes(x=hour, y=count,fill=..x..)) +
      geom_bar(stat="identity") +
      labs(x="Hour", y="Total Rides", title="Total Rides by Hour of the Day") +
      scale_fill_gradientn(labels=NULL, colors=c("orangered2", "yellow", "blue2"))+
      theme(legend.position = "none") +
      scale_y_continuous(labels = comma, breaks = seq(0, 900000, 100000)) +
      scale_x_continuous(breaks = seq(0, 23, 1))
  } else {
    ggplot(hourdata, aes(x=hour, y=count, fill=..x..)) +
      geom_bar(stat="identity") +
      labs(x="Hour", y="Total Rides", title="Total Rides by Hour of the Day") +
      scale_fill_gradientn(labels=NULL, colors=c("orangered2", "yellow", "blue2"))+
      theme(legend.position = "none") +
      scale_y_continuous(labels = comma, breaks = seq(0, 900000, 100000)) +
      scale_x_continuous(breaks = seq(0, 23, 1),
                         labels =c("12am","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm"),
                         guide = guide_axis(angle = 40))
  }
  
}


# plot distribution of rides by month
monthplot1 <- function() {
  
  monthdata <- read.table("MONTHS.csv", header=TRUE,sep=",")
  
    ggplot(monthdata, aes(x=month, y=count)) +
    geom_bar(stat="identity", fill="palegreen3") +
    scale_y_continuous(labels = comma, breaks = seq(0, 1200000, 200000)) +
    scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    labs(x="Month", y="Total Rides", title="Total Rides by Month")
  
}


#  plot dist of rides by day of the week
dayplot1 <- function() {
  
  daydata <- read.table("WDAY.csv", header=TRUE, sep=",")
  
  ggplot(daydata, aes(x=wday, y=count)) +
    geom_bar(stat="identity", fill="skyblue3") +
    scale_y_continuous(labels = comma, breaks = seq(0, 2000000, 250000)) +
    scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
    labs(x="Day of the Week", y="Total Rides", title="Total Rides by Day of the Week")
  
}


# plot dist of rides by binned mileage
milesplot1 <- function(mode) {
  
  miles1 <- read.table("MILES1.csv", header=TRUE)
  miles2 <- read.table("MILES2.csv", header=TRUE)
  miles3 <- read.table("MILES3.csv", header=TRUE)
  miles4 <- read.table("MILES4.csv", header=TRUE)
  miles5 <- read.table("MILES5.csv", header=TRUE)
  miles6 <- read.table("MILES6.csv", header=TRUE)
  miles7 <- read.table("MILES7.csv", header=TRUE)
  miles8 <- read.table("MILES8.csv", header=TRUE)
  miles9 <- read.table("MILES9.csv", header=TRUE)
  miles10 <- read.table("MILES10.csv", header=TRUE)
  miles11 <- read.table("MILES11.csv", header=TRUE)
  miles12 <- read.table("MILES12.csv", header=TRUE)
  
  milesdata <-rbind(miles1,miles2,miles3,miles4,miles5,miles6,miles7,miles8,miles9,miles10,miles11,miles12)
  
  if(mode=="miles") {
    breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
    tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
              "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
    group_tags <- cut(milesdata$miles, 
                      breaks=breaks, 
                      include.lowest=TRUE, 
                      right=FALSE, 
                      labels=tags)
  
    ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
      geom_bar(width = 0.9) +
      scale_fill_gradientn(colours = c("plum1","mediumpurple3","darkorchid3")) +
      labs(x="Miles", y="Total Rides", title="Number of Rides by Mileage") +
      scale_y_continuous(labels = comma) +
      theme(legend.position = "none") 
  } else {
    
    milesdata$kim <- (milesdata$miles*1.6)
    
    breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
    tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
              "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
    group_tags <- cut(milesdata$kim, 
                      breaks=breaks, 
                      include.lowest=TRUE, 
                      right=FALSE, 
                      labels=tags)
    
    ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
      geom_bar(width = 0.9) +
      scale_fill_gradientn(colours = c("plum1","mediumpurple3","darkorchid3")) +
      labs(x="Kilometers", y="Total Rides", title="Number of Rides by Kilometers") +
      scale_y_continuous(labels = comma) +
      theme(legend.position = "none") 
  }
  
}


# plot dist of rides by binned trip time
secsplot1 <- function() {
  
  secs1 <- read.table("SECS1.csv", header=TRUE)
  secs2 <- read.table("SECS2.csv", header=TRUE)
  secs3 <- read.table("SECS3.csv", header=TRUE)
  secs4 <- read.table("SECS4.csv", header=TRUE)
  secs5 <- read.table("SECS5.csv", header=TRUE)
  secs6 <- read.table("SECS6.csv", header=TRUE)
  secs7 <- read.table("SECS7.csv", header=TRUE)
  secs8 <- read.table("SECS8.csv", header=TRUE)
  secs9 <- read.table("SECS8.csv", header=TRUE)
  secs10 <- read.table("SECS10.csv", header=TRUE)
  secs11 <- read.table("SECS11.csv", header=TRUE)
  secs12 <- read.table("SECS12.csv", header=TRUE)
  
  secsdata <- rbind(secs1,secs2,secs3,secs4,secs5,secs6,secs7,secs8,secs9,secs10,secs11,secs12)

  breaks <- c(0, 180, 300, 420, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 10800, 14400, 18000)
  tags <- c("3min","5min","7min","10min","12min","15min","20min","30min","45min","1hr","2hr","3hr","4hr","5hr")

  group_tags <- cut(secsdata$secs,
                    breaks=breaks,
                    include.lowest=TRUE,
                    right=FALSE,
                    labels=tags)
  
  ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
    geom_bar() +
    labs(x="Trip time", y="Count", title="Number of Rides by Trip Time") +
    scale_y_continuous(labels = comma) +
    theme(legend.position = "none")
}



