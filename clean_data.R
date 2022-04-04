library(lubridate)

df <- read.table("taxi_data_all.csv", sep=",", header=TRUE, quote="\"")

colnames(df) <- c("time", "secs", "miles", "pickup", "dropoff", "company")

df$timeNew <- strptime(df$time, format="%m/%d/%Y %I:%M:%S %p", tz="GMT")

df$date <- as.Date(df$timeNew)

df$hour <- hour(df$timeNew)

df$time <- NULL
df$timeNew <- NULL

df$year <- year(df$date)
df$month <- month(df$date)

df$day <- wday(df$date, label=TRUE)

write.table(df, "taxi_data_clean.tsv", sep="\t", row.names = FALSE)
