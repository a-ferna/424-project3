
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






