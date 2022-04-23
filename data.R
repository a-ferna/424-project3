library(scales)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)


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



getCommNames <- function() {
  community_area <- read.table("community_areas.csv", header=TRUE, sep=",")
  alpha <- community_area[order(community_area$community),]
  
  return(alpha$community)
  

}

getAreas <- function(){
  
  community_area <- read.table("community_areas.csv", header=TRUE, sep=",")
  alpha <- community_area[order(community_area$community),]
  return(alpha)
}

getCompNames <- function() {
  companies <- read.table("companies.csv", header=TRUE, sep=",")
  alpha <- companies[order(companies$company),]
  return(alpha$company)
  
}
getComp <- function() {
  companies <- read.table("companies.csv", header=TRUE, sep=",")
  alpha <- companies[order(companies$company),]
  return(alpha)
}



## function to upload file given areaID
getDATADrop <- function(areaID) {
  areaID = areaID
  # string1 <- "/Users/andreaherrera/Downloads/project2-cs424/code/data_files/final_data_"
  #string1 <- "/Users/ariad/Documents/424/project2/data_files/final_data_"
  string1 <- "dropoff_areas/dropoffarea"
  
  string2 <- as.character(areaID)
  path = paste(string1,string2,".tsv",sep="")
  print(path)
  Data <- read.table(path, sep = ',', header = TRUE, quote="\"")
  
  return(Data)
}

#test 
#areaTest <-getDATADrop(1)
#areaTestPick <-getDATAPick(1)



## function to upload file given areaID
getDATAPick <- function(areaID) {
  areaID = areaID

  string1 <- "pickup_areas/pickuparea"
  
  string2 <- as.character(areaID)
  path = paste(string1,string2,".tsv",sep="")
  print(path)
  Data <- read.table(path, sep = ',', header = TRUE, quote="\"")
  
  return(Data)
}


## function to upload file given areaID and company id
getDATA2Drop <- function(areaID,companyID) {
  areaID = areaID
  companyID = companyID
  string0 <- as.character(companyID)
  string1 <- "companydropoff/"
  stringM <- "companydropoff"
  
  string2 <- as.character(areaID)
  path = paste(string1,string0,stringM,string2,".tsv",sep="")
  print(path)
  Data <- read.table(path, sep = ',', header = TRUE, quote="\"")
  
  return(Data)
}

#test 
#areaTest <-getDATADrop(1)
#areaTestPick <-getDATAPick(1)



## function to upload file given areaID
getDATA2Pick <- function(areaID,companyID) {
  areaID = areaID
  companyID = companyID
  string0 <- as.character(companyID)
  string1 <- "companypickup/"
  stringM <- "companypickup"
  string2 <- as.character(areaID)
  
  path = paste(string1,string0,stringM,string2,".tsv",sep="")
  print(path)
  Data <- read.table(path, sep = ',', header = TRUE, quote="\"")
  
  return(Data)
}

#areaTestPick <-getDATA2Pick(1,1)

# dist number of rides by day of year
dates <- data.frame(alldata$date)
colnames(dates) <- c("date")

ggplot(dates, aes(x=date)) +
  geom_histogram(binwidth=.5) +
  geom_density(alpha=.2, fill="red")  #density doesn't show
  

# dist number of rides by hour of the day -24 hr 
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
 

# dist number of rides by day of the week
ggplot(alldata, aes(x=wday)) +
  geom_bar(stat="count", fill="skyblue3") +
  scale_y_continuous(labels = comma, breaks = seq(0, 2000000, 250000)) +
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  labs(x="Day of the Week", y="Total Rides", title="Total Rides by Day of the Week")


# dist number of rides by month of the year
ggplot(alldata, aes(x=month)) +
  geom_bar(stat="count", fill="palegreen3") +
  scale_y_continuous(labels = comma, breaks = seq(0, 1200000, 200000)) +
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(x="Month", y="Total Rides", title="Total Rides by Month")


# dist number of rides by binned mileage
breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
          "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
group_tags <- cut(alldata$miles, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
# summary(group_tags)

ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
  geom_bar(width = 0.9) +
  scale_fill_gradientn(colours = c("plum1","mediumpurple3","darkorchid3")) +
  labs(x="Miles", y="Total Rides", title="Number of Rides by Mileage") +
  scale_y_continuous(labels = comma) +
  # scale_x_discrete(guide = guide_axis(angle = 10)) +
  theme(legend.position = "none") 


# dist number of rides by binned trip time
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
## still need work so that the end of the graph shows
## how to create table/dataframe from this?
  

##############################################################################
companies <- read.table("companies.csv", header=TRUE, sep=",")

# map company name to company id
# companies[companies$id==company_id, "company"]

company_id <- 4

string1 <- "company_files/company"
string2 <- as.character(company_id)
path = paste(string1,string2,".tsv",sep="")
print(path)
company <- read.table(path, sep = ',', header = TRUE, quote="\"")


# dist number of rides by day of year
company$date <- as.Date(with(company, paste("2019", month, day, sep="-")), "%Y-%m-%d")

name <- companies[companies$id==company_id, "company"]

ggplot(company, aes(x=date)) +
  geom_bar(stat="count", width=0.9, fill="orange2") +
  labs(x="Date", y="Count", title=paste("Number of Rides by",name,"in 2019", sep=" "))


# dist number of rides by hour of the day -24 hr 
ggplot(company, aes(x=hour, fill=..x..)) +
  geom_bar(stat="count") +
  labs(x="Hour", y="Number of Rides", title="Number Rides by Hour of the Day") +
  scale_fill_gradientn(labels=NULL, colors=c("orangered2", "yellow", "blue2"))+
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(0, 23, 1))
# scale_x_continuous(breaks = seq(0, 23, 1),
#                    labels =c("12am","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm"),
#                    guide = guide_axis(angle = 40))


# dist number of rides by day of the week
ggplot(company, aes(x=wday)) +
  geom_bar(stat="count", fill="skyblue3") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  labs(x="Day of the Week", y="Number of Rides", title="Total Rides by Day of the Week")


# dist number of rides by month of the year
ggplot(company, aes(x=month)) +
  geom_bar(stat="count", fill="palegreen3") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(x="Month", y="Number of Rides", title="Number of Rides by Month")


# dist number of rides by binned mileage
breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
          "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
group_tags <- cut(company$miles, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)

ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
  geom_bar(width = 0.9) +
  scale_fill_gradientn(colours = c("plum1","mediumpurple3","darkorchid3")) +
  labs(x="Miles", y="Total Rides", title="Number of Rides by Mileage") +
  scale_y_continuous(labels = comma) +
  # scale_x_discrete(guide = guide_axis(angle = 10)) +
  theme(legend.position = "none") 


# dist number of rides by binned trip time
breaks <- c(0, 180, 300, 420, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 10800, 14400, 18000)
tags <- c("3min","5min","7min","10min","12min","15min","20min","30min","45min","1hr","2hr","3hr","4hr","5hr")
# breaks <- c(0, 100, 200, 300, 400, 500, 600, 750, 1000, 1500, 2000, 3000, 5000, 10000, 15000, 18000)
# tags <- c("60-100","100-200","200-300","300-400","400-500","500-600","600-750","750-1000","1000-1500","1500-2000","2000-3000", "3000-5000", "5000-10000", "10000-15000", "15-18000")

group_tags <- cut(company$secs,
                  breaks=breaks,
                  include.lowest=TRUE,
                  right=FALSE,
                  labels=tags)

ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
  geom_bar() +
  labs(x="Trip time", y="Count", title="Number of Rides by Trip Time") +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")

########################################################
areas <- read.table("community_areas.csv", header=TRUE, sep=",")

# dir <- "pickup" 
dir <- "dropoff"
comm_num <- 6 #user chosen community are map to its id

string1 <- paste(dir, "_areas/", dir, "area", sep="")
string2 <- as.character(comm_num)
path = paste(string1,string2,".tsv",sep="")
print(path)
areaData <- read.table(path, sep = ',', header = TRUE, quote="\"")

# dist number of rides by day of year
areaData$date <- as.Date(with(areaData, paste("2019", month, day, sep="-")), "%Y-%m-%d")

name <- areas[areas$area_id==comm_num, "community"]
name <- str_to_title(name)

ggplot(areaData, aes(x=date)) +
  geom_bar(stat="count", width=0.8, fill="orange2") +
  labs(x="Date", y="Number of rides", title=paste("Number of rides with",dir,"location",name, sep=" ")) +
  scale_y_continuous(labels = comma)


# dist number of rides by hour of the day -24 hr 
ggplot(areaData, aes(x=hour, fill=..x..)) +
  geom_bar(stat="count") +
  labs(x="Hour", y="Number rides", title=paste("Number rides by the hour in",name,"as",dir,"location",sep=" ")) +
  scale_fill_gradientn(labels=NULL, colors=c("orangered2", "yellow", "blue2"))+
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  # scale_x_continuous(breaks = seq(0, 23, 1))
  scale_x_continuous(breaks = seq(0, 23, 1),
                     labels =c("12am","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm"),
                     guide = guide_axis(angle = 40))

# dist number of rides by day of the week
ggplot(areaData, aes(x=wday)) +
  geom_bar(stat="count", fill="skyblue3") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  labs(x="Day of the Week", y="Number of Rides", title=paste("Number of rides by day of the week in",name,"as",dir,"location",sep=" "))

# dist number of rides by month of the year
ggplot(areaData, aes(x=month)) +
  geom_bar(stat="count", fill="palegreen3") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(x="Month", y="Number of rides", title=paste("Number of rides by month in",name,"as",dir,"location",sep=" "))


# dist number of rides by binned mileage
breaks <- c(0, 0.75, 1, 1.25, 1.5, 2, 3, 5, 8, 10, 15, 20, 25, 30, 40, 101)
tags <- c("[0.5-0.75]","[0.75-1]","[1-1.25]","[1.25-1.5]","[1.5-2]","[2-3]","[3-5]","[5-8]","[8-10]","[10-15]",
          "[15-20]","[20-25]","[25-30]","[30-40]","[40-100]")
group_tags <- cut(areaData$miles, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)

ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
  geom_bar(width = 0.9) +
  scale_fill_gradientn(colours = c("plum1","mediumpurple3","darkorchid3")) +
  labs(x="Miles", y="Number Rides", title=paste("Number of rides by Mileage at",name,dir,"location",sep=" ")) +
  scale_y_continuous(labels = comma) +
  # scale_x_discrete(guide = guide_axis(angle = 10)) +
  theme(legend.position = "none") 


# dist number of rides by binned trip time
breaks <- c(0, 180, 300, 420, 600, 720, 900, 1200, 1800, 2700, 3600, 7200, 10800, 14400, 18000)
tags <- c("3min","5min","7min","10min","12min","15min","20min","30min","45min","1hr","2hr","3hr","4hr","5hr")
# breaks <- c(0, 100, 200, 300, 400, 500, 600, 750, 1000, 1500, 2000, 3000, 5000, 10000, 15000, 18000)
# tags <- c("60-100","100-200","200-300","300-400","400-500","500-600","600-750","750-1000","1000-1500","1500-2000","2000-3000", "3000-5000", "5000-10000", "10000-15000", "15-18000")

group_tags <- cut(areaData$secs,
                  breaks=breaks,
                  include.lowest=TRUE,
                  right=FALSE,
                  labels=tags)

ggplot(data = as_tibble(group_tags), mapping=aes(x=value, fill=..x..)) +
  geom_bar() +
  labs(x="Trip time", y="Count", title=paste("Number of Rides by Trip Time at",name, dir,"location", sep=" ")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")

