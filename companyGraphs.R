
getCompIDs <- function() {
  
  comps <- read.table("companies.csv", header=TRUE, sep=",")
  return(comps)
  
}


getCompanyData <- function(company_id) {
  
  string1 <- "company_files/company"
  string2 <- as.character(company_id)
  path = paste(string1,string2,".tsv",sep="")
  # print(path)
  company <- read.table(path, sep = ',', header = TRUE, quote="\"")

  company$date <- as.Date(with(company, paste("2019", month, day, sep="-")), "%Y-%m-%d")
  return(company)
  
}


yearplot3 <- function(data, name) {
  
  ggplot(data, aes(x=date)) +
    geom_bar(stat="count", width=0.9, fill="orange2") +
    labs(x="Date", y="Count", title=paste("Number of Rides by",name,"in 2019", sep=" "))
}


hourplot3 <- function(data, mode, name) {
  
  if(mode=="24H") {
    
    # dist of rides by hour of the day 
    ggplot(data, aes(x=hour, fill=..x..)) +
      geom_bar(stat="count") +
      labs(x="Hour", y="Number of Rides", title=paste("Number rides by hour of the day -",name, sep=" ")) +
      scale_fill_gradientn(labels=NULL, colors=c("orangered2", "yellow", "blue2"))+
      theme(legend.position = "none") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks = seq(0, 23, 1))
  } else {
    # dist of rides by hour of the day -24 hr
    ggplot(data, aes(x=hour, fill=..x..)) +
      geom_bar(stat="count") +
      labs(x="Hour", y="Number of Rides", title=paste("Number rides by hour of the day -",name, sep=" ")) +
      scale_fill_gradientn(labels=NULL, colors=c("orangered2", "yellow", "blue2"))+
      theme(legend.position = "none") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks = seq(0, 23, 1),
                         labels =c("12am","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm"),
                         guide = guide_axis(angle = 40))

  }
}

monthplot3 <- function(data, name) {
  
  # dist of rides by month of the year
  ggplot(data, aes(x=month)) +
    geom_bar(stat="count", fill="palegreen3") +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    labs(x="Month", y="Number of Rides", title=paste("Number of rides by month -",name,sep=" "))
  
}


dayplot3 <- function(data, name) {
  
  # dist number of rides by day of the week
  ggplot(data, aes(x=wday)) +
    geom_bar(stat="count", fill="skyblue3") +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
    labs(x="Day of the Week", y="Number of Rides", title=paste("Rides by day of the week -",name,sep=" "))
}

milesplot3 <- function(data, name, mode) {
  
  if(mode=="miles") {
    # dist of rides by binned mileage
    breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
    tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
              "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
    group_tags <- cut(data$miles,
                      breaks=breaks,
                      include.lowest=TRUE,
                      right=FALSE,
                      labels=tags)
  
    ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
      geom_bar(width = 0.9) +
      scale_fill_gradientn(colours = c("plum1","mediumpurple3","darkorchid3")) +
      labs(x="Miles", y="Total Rides", title=paste("Number of rides by mileage -",name,sep=" ")) +
      scale_y_continuous(labels = comma) +
      # scale_x_discrete(guide = guide_axis(angle = 10)) +
      theme(legend.position = "none")
  } else {
    
    data$kim <- (data$miles*1.6)
    
    # dist of rides by binned mileage
    breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
    tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
              "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
    group_tags <- cut(data$kim,
                      breaks=breaks,
                      include.lowest=TRUE,
                      right=FALSE,
                      labels=tags)
    
    ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
      geom_bar(width = 0.9) +
      scale_fill_gradientn(colours = c("plum1","mediumpurple3","darkorchid3")) +
      labs(x="Kilometers", y="Total Rides", title=paste("Number of rides by Kilometers -",name,sep=" ")) +
      scale_y_continuous(labels = comma) +
      # scale_x_discrete(guide = guide_axis(angle = 10)) +
      theme(legend.position = "none")
    
  }

}

secsplot3 <- function(data, name) {
  
  # dist of rides by binned trip time
  breaks <- c(0, 180, 300, 420, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 10800, 14400, 18000)
  tags <- c("3min","5min","7min","10min","12min","15min","20min","30min","45min","1hr","2hr","3hr","4hr","5hr")

  group_tags <- cut(data$secs,
                    breaks=breaks,
                    include.lowest=TRUE,
                    right=FALSE,
                    labels=tags)

  ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
    geom_bar() +
    labs(x="Trip time", y="Count", title=paste("Number of rides by trip time -",name,sep=" ")) +
    scale_y_continuous(labels = comma) +
    theme(legend.position = "none")

}



