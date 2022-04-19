library(scales)
library(RColorBrewer)


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



getComm <- function() {
  set.seed(1)
  
  community_area <- read.table("areas/community_areas.csv", header=TRUE, sep=",")
  alpha <- community_area[order(community_area$community),]
  
  return(alpha$community)
  
}


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
ggplot(alldata, aes(x=secs))+
  geom_histogram(binwidth = 0.5)
