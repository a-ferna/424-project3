

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
  set.seed(1)
  
  community_area <- read.table("community_areas.csv", header=TRUE, sep=",")
  alpha <- community_area[order(community_area$community),]
  
  return(alpha$community)
  

}

getAreas <- function(){
  
  community_area <- read.table("community_areas.csv", header=TRUE, sep=",")
  alpha <- community_area[order(community_area$community),]
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
  # string1 <- "/Users/andreaherrera/Downloads/project2-cs424/code/data_files/final_data_"
  #string1 <- "/Users/ariad/Documents/424/project2/data_files/final_data_"
  string1 <- "pickup_areas/pickuparea"
  
  string2 <- as.character(areaID)
  path = paste(string1,string2,".tsv",sep="")
  print(path)
  Data <- read.table(path, sep = ',', header = TRUE, quote="\"")
  
  return(Data)
  
  
}


getCommNames <- function() {
  
  community_area <- read.table("community_areas.csv", header=TRUE, sep=",")
  alpha <- community_area[order(community_area$community),]
  
  return(alpha$community)
  
}

getComp <- function() {
  companies <- read.table("companies.csv", header=TRUE, sep=",")
  alpha <- companies[order(companies$company),]
  return(alpha)
}



getCompNames <- function() {
  companies <- read.table("companies.csv", header=TRUE, sep=",")
  alpha <- companies[order(companies$company),]
  return(alpha$company)
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



