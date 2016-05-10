############## Intro ############### 

## Created 9/15 and updated 5/16 by Daniel Hadley to load and visualize data from various sources ##
# and make charts for somervillema.gov/dashboard/daily.html 

# Basically, this crunches the data and 'knits' it into an HTML file using knitr. The HTML relies on highcharts for the charts and leaflet for the maps. Also, it uploads the HTML and geojson files to our production server. 

# For the theory, see http://danielphadley.com/How-To-Dashboard-R/


## working Directory, packages, and variables for the dates ##
setwd("c:/Users/dhadley/Documents/GitHub/Somerville_Daily_Dashboard/")

# This pulls in the credentials you need
# And the dashboard function made by DH
source("./config.R")
source("./My_Dashboard_Functions.R")


library(RCurl)
library(dplyr)
library(tidyr)
library(lubridate)
library(httr) # Upload to Socrata
# library(knitr) loads plyr and causes conflicts so I load it later
library(leafletR) # to make geojson


## dates ##
today <- Sys.Date()
yesterday <- today - 1

# custom function for dealing with dates
DateVariableMaker <- function(MyData, MyDataDate){
  
  #Takes the date from data and adds important variables
  
  MyData$Date <- as.Date(MyData[,MyDataDate])
  MyData$Year.Month <- format(MyData$Date, '%Y-%m')
  MyData$Month <- format(MyData$Date, '%m')
  MyData$Year <- format(MyData$Date, '%Y')
  MyData$DaysAgo <- difftime(MyData$Date, today, units = "days")
  
  return(MyData)
}




############## Police ############### 

## Read Data and make new Variables ##
ci <- read.csv("//fileshare1/Departments2/Somerstat Data/Police/daily/CriminalIncidents.csv")
qol <- read.csv("//fileshare1/Departments2/Somerstat Data/Police/daily/QualityOfLife.csv")


# Clean
ci$DateTime <- parse_date_time(ci$dtreported, orders = "%m-%d-%y %I:%M:%S %p")
ci$Date <- format(ci$DateTime, '%Y-%m-%d')
ci <- add_date_vars(ci, "Date")



## Time series
TimeSeries_ci <- make_x_day_ts_multiple_v(ci, "Date", 7, "offense")

# Get it down to about 17 weeks and format a couple things
TimeSeries_ci <- TimeSeries_ci %>% 
  tail(17) %>% 
  mutate(period_ending = format(period_ending, format = "%b %d")) %>% 
  data.frame() # the other format was throwing off printing



## CI map
forMap_ci <- ci %>% 
  filter(days_ago > -15 & X != "" & X != 0) %>% 
  select(Y, X, offense, Date) %>% 
  rename(latitude = Y, longitude = X)

# Convert and upload to our server
toGeoJSON(forMap_ci, "PoliceCI", "./tmp/")

ftpUpload(what = "./tmp/PoliceCI.geojson",
          to = paste(Somerville_server, "PoliceCI.geojson", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/geo/daily/")




############## 311 ############### 

## Read Data and make new Variables ##
cs <- read.csv("//fileshare1/Departments2/Somerstat Data/Constituent_Services/data/311_Somerville.csv")


# returns string w/o leading or trailing whitespace
# I was having trouble with trailing spaces, e.g. "Graffiti "
# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
cs <- as.data.frame(apply(cs,2,function (cs) sub("\\s+$", "", cs)))

# don't know why this was stored as an integer
cs$typeName <- as.character(cs$typeName)

cs$Date <- as.Date(cs$displayDate, "%m/%d/%Y")
cs <- DateVariableMaker(cs, "Date")




#### Find statistical time-series anomalies & make a chart and map of top 3 ####

# Make geojson & chart for the Unique Top 3 map #
# Put it in a function so as to keep code clean

## First the algo to find which work orders had the largest (statistical) increase last week
TSAnomalies <- function(){
  
  ##  Find calls that have increased in statistically significant ways (except small n) ##
  
  days <- cs %>%
    filter(DaysAgo > -120) %>%
    filter(secondary_issue_type == "Service Requests") %>% 
    filter(typeName != "Miscellaneous" & typeName != "Welcome desk information" & 
             typeName != "CS-Lost call (wrong #, hang up, dead air)") %>% 
    group_by(Date, typeName) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(typeName, count)
  
  allDays <- seq.Date(from=days$Date[1], to = today, b='days')
  allDays <- allDays  %>%  as.data.frame() 
  colnames(allDays)[1] = "Date"
  
  # After this we will have a df with every date and how many work orders
  ts = merge(days, allDays, by='Date', all=TRUE)
  ts[is.na(ts)] <- 0
  
  ## Now weekly
  # First we get the day number of yesterday for the weekending date
  # Because otherwise it ends on Sunday
  # http://stackoverflow.com/questions/8030812/how-can-i-group-days-into-weeks/8031372#8031372
  WeekEndingNum <- as.numeric(ts$Date[nrow(ts)])
  
  # Ok now we can group by a week that ended yesterday and summarise
  tsWeekly <- ts %>% 
    mutate(Week = (WeekEndingNum - as.numeric(Date)) %/% 7) %>% 
    group_by(Week) %>%
    summarise_each(funs(sum, max)) %>% #max to get the last date
    arrange(-Week)
  
  tsWeekly <- tsWeekly[-1,] # Drop first row because it's an incomplete week
  
  # Find and remove small n - work order types where there was only 3 or fewer the whole week
  lastRow <- tail(tsWeekly, 1)
  # Now subset based on the value of hte last row, which is the total for the last week
  tsWeekly <- tsWeekly[,lastRow > 3]
  
  # all weeks expressed as a Z-Score
  tsWeeklyZ <- tsWeekly %>% 
    select(Date_sum:Date_max) %>% 
    select(-Date_sum, -Date_max) %>% 
    scale() %>% 
    data.frame()
  
  ToGetNames <- tsWeekly %>% 
    select(Date_sum:Date_max) %>% 
    select(-Date_sum, -Date_max)
  
  Increaes <- tsWeeklyZ %>% 
    tail(n = 1) %>% 
    t() %>% 
    as.data.frame()
  
  row.names(Increaes) <- names(ToGetNames)
  
  Increaes <- cbind(typeName = rownames(Increaes), Increaes)
  names(Increaes) <- c("typeName", "ZScore")
  Increaes$typeName <- gsub("_sum", "", Increaes$typeName)
  
  #Sort by Z-Score 
  Increaes <- arrange(Increaes, -ZScore)
  
  #Top Three
  unique_cs_top_three <- c(as.character(Increaes$typeName[1]),
                           as.character(Increaes$typeName[2]),
                           as.character(Increaes$typeName[3]))
  
  TopThreeIncreases_cs_all <- cs %>% 
    filter(typeName %in% unique_cs_top_three)
  
  
  return(TopThreeIncreases_cs_all)
  
}

TopThreeIncreases_cs_all <- TSAnomalies() # We use this to build the gauges too

TopThreeIncreases_cs <- TopThreeIncreases_cs_all %>% 
  select(latitude, longitude, typeName, Date, DaysAgo, comments)


# First the time series chart
TimeSeriesMaker <- function(){
  
  ##  Turns the data into a complete time series ##
  
  days <- TopThreeIncreases_cs %>%
    filter(DaysAgo > -120) %>%
    group_by(Date, typeName) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(typeName, count)
  
  allDays <- seq.Date(from=days$Date[1], to = today, b='days')
  allDays <- allDays  %>%  as.data.frame() 
  colnames(allDays)[1] = "Date"
  
  # After this we will have a df with every date and how many work orders
  ts = merge(days, allDays, by='Date', all=TRUE)
  ts[is.na(ts)] <- 0
  
  ## Now weekly
  # First we get the day number of yesterday for the weekending date
  # Because otherwise it ends on Sunday
  # http://stackoverflow.com/questions/8030812/how-can-i-group-days-into-weeks/8031372#8031372
  WeekEndingNum <- as.numeric(ts$Date[nrow(ts)])
  
  # Ok now we can group by a week that ended yesterday and summarise
  tsWeekly <- ts %>% 
    mutate(Week = (WeekEndingNum - as.numeric(Date)) %/% 7) %>% 
    group_by(Week) %>%
    summarise_each(funs(sum, max)) %>% #max to get the last date
    arrange(-Week)
  
  tsWeekly <- tsWeekly[-1,] # Drop first row because it's an incomplete week
  
  tsWeekly <- tsWeekly %>% 
    select(Date_sum:Date_max) %>% 
    select(-Date_sum)
  
  tsWeekly$Date_max <- format(tsWeekly$Date_max, format = "%b %d")
  
  names(tsWeekly) <- gsub("_sum", "", names(tsWeekly))
  
  return(tsWeekly)
  
}

forChart_TopThree_cs <- TimeSeriesMaker()
forChart_TopThree_cs <- as.data.frame(forChart_TopThree_cs) # was having problems kniting


# Then the Map
forMap_cs <- TopThreeIncreases_cs %>% 
  filter(DaysAgo > -8 & latitude != "" & latitude != 0) %>% 
  mutate(# Charcs messing up geojson
    comments = gsub("\"","", comments),
    comments = gsub("\t", " ", comments),
    comments = gsub('"', '', comments)) %>% 
  select(-DaysAgo)


# This is a variable in the daily.rhtml
unique_cs_top <- as.character(forMap_cs$typeName[1])




#### Pie Charts of time-series anomalies, open to closed ####

PortionOpen <- TopThreeIncreases_cs_all %>% 
  group_by(typeName, LastAction) %>% 
  summarise(n = n()) %>%
  data.frame() %>%
  spread(LastAction, n)

PortionOpen[is.na(PortionOpen)] <- 0
PortionOpen$Total <- rowSums(PortionOpen[2:ncol(PortionOpen)])
PortionOpen$Open <- PortionOpen$Total - PortionOpen$Closed




#### Median hours open of time-series anomalies #### 

HoursOpen <- TopThreeIncreases_cs_all %>%
  # Use lubridate to get R to convert am/pm into euro time
  mutate(dateParsed = mdy_hm(displayDate),
         dateLastActionParsed = mdy_hm(dateLastAction))

# Days open: if it's closed, using the closing date, otherwise use the date from the very last work order recorded
HoursOpen$TimeOpen <- ifelse(HoursOpen$LastAction == "Closed", HoursOpen$dateLastActionParsed - HoursOpen$dateParsed, max(HoursOpen$dateLastActionParsed) - HoursOpen$dateParsed)

# Add one second for stats
HoursOpen$TimeOpen <- HoursOpen$TimeOpen + 1

# Convert to days
HoursOpen$DaysOpen <- ((HoursOpen$TimeOpen / 60) / 1440)
HoursOpen$HoursOpen <- ((HoursOpen$TimeOpen / 60) / 60)
  
# Scientific notation is annoying when reviewing these
options(scipen=999)
  
HoursOpen <- HoursOpen %>%
  group_by(typeName) %>% 
  summarise(HoursOpen = median(HoursOpen), n = n())




#### Top from last day ####
TopFifteen_cs <- cs %>% 
  filter(DaysAgo > -2) %>%
  # Take out internal ones
  filter(secondary_issue_type != "internally generated") %>% 
  group_by(typeName) %>% 
  dplyr::summarise(count=n()) %>% 
  arrange(-count) %>% 
  filter(typeName != "Miscellaneous" & typeName != "Welcome desk information" & typeName != "CS-Lost call (wrong #, hang up, dead air)")
  

# If more than 15, mow it down
if(nrow(TopFifteen_cs) > 15 ){
  TopFifteen_cs <- TopFifteen_cs[1:15,]
}

# Make a top 5 too
TopFive_cs <- arrange(TopFifteen_cs[1:5,])

# Arrange them for the charts
TopFifteen_cs <- arrange(TopFifteen_cs, count)
TopFive_cs <- arrange(TopFive_cs, count)




#### Yesterday compared to average ####

# to get the count of calls yesterday
Yesterday_cs <- cs %>%
  filter(secondary_issue_type != "internally generated") %>% 
  filter(DaysAgo > -2 & DaysAgo < 0)

# Get average for similar type days 
yesterdayType <-  ifelse(wday(yesterday) == 1, "weekend",
                         ifelse(wday(yesterday) == 7, "weekend", 
                                "weekday"))

cs$DayType <- ifelse(wday(cs$Date) == 1, "weekend",
                     ifelse(wday(cs$Date) == 7, "weekend", 
                            "weekday"))

CompareAverage <- function(){
  
  ## Compares week days to week days and weekends to weekends
  # To see if yesterday was average for calls 
  
  cs_comporable <- cs %>% 
    filter(DayType == yesterdayType) %>% 
    filter(secondary_issue_type != "internally generated") %>%
    group_by(Date) %>% 
    summarise(n = n())
  
  averageCallNumber <- mean(cs_comporable$n)
  stdev <- sd(cs_comporable$n)
  
  CallNumberYesterday <- nrow(Yesterday_cs)
  delta <- CallNumberYesterday - averageCallNumber 
  
  comparison <- ifelse(delta > 0 & delta > stdev, "significantly above average",
                       ifelse(delta > 0 & delta < stdev, "slightly above average",
                              ifelse(delta < 0 & abs(delta) > stdev, "significantly below average",
                                     ifelse(delta < 0 & abs(delta) < stdev, "slightly below average",
                                            "average"))))
  return(comparison)
}

comparisonCS <- CompareAverage()




#### Quality of Life ####

# time series last 120 days
TimeSeriesMaker_qol <- function(){
  
  days <- cs %>% 
    filter(DaysAgo > -120) %>%
    filter(secondary_issue_type != "internally generated") %>% 
    filter(typeName == "Rats" | typeName == "Graffiti" | typeName == "Pothole") %>%
    group_by(Date, typeName) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    spread(typeName, count)
  
  allDays <- seq.Date(from=days$Date[1], to = days$Date[nrow(days)], b='days')
  allDays <- allDays  %>%  as.data.frame() 
  colnames(allDays)[1] = "Date"
  
  # After this we will have a df with every date and how many work orders
  ts = merge(days, allDays, by='Date', all=TRUE)
  ts[is.na(ts)] <- 0
  
  ## Now weekly
  # First we get the day number of yesterday for the weekending date
  # Because otherwise it ends on Sunday
  # http://stackoverflow.com/questions/8030812/how-can-i-group-days-into-weeks/8031372#8031372
  WeekEndingNum <- as.numeric(ts$Date[nrow(ts)])
  
  # Ok now we can group by a week that ended yesterday and summarise
  tsWeekly <- ts %>% 
    mutate(Week = (WeekEndingNum - as.numeric(Date)) %/% 7) %>% 
    group_by(Week) %>%
    summarise_each(funs(sum, max)) %>% #max to get the last date
    arrange(-Week)
  
  tsWeekly <- tsWeekly[-1,] # Drop first row because it's an incomplete week
  
  tsWeekly <- tsWeekly %>% 
    select(Date_sum:Date_max) %>% 
    select(-Date_sum)
  
  tsWeekly$Date_max <- format(tsWeekly$Date_max, format = "%b %d")
  
  names(tsWeekly) <- gsub("_sum", "", names(tsWeekly))
  
  return(tsWeekly)
  
}

forTS_qol <- TimeSeriesMaker_qol()


## Make geojson for the top quality-of-life calls ##
forMap_qol <- cs %>% 
  filter(DaysAgo > -8) %>% 
  filter(secondary_issue_type != "internally generated") %>% 
  filter(typeName == "Rats" | typeName == "Graffiti" | typeName == "Pothole") %>%
  select(latitude, longitude, typeName)




#### Convert to geojson and put it on our server ####

toGeoJSON(forMap_cs, "UniqueCS", "./tmp/")

toGeoJSON(forMap_qol, "QualityOfLifeCS", "./tmp/")

ftpUpload(what = "./tmp/UniqueCS.geojson",
          to = paste(Somerville_server, "UniqueCS.geojson", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/geo/daily/")

ftpUpload(what = "./tmp/QualityOfLifeCS.geojson",
          to = paste(Somerville_server, "QualityOfLifeCS.geojson", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/geo/daily/")




#### Top internally-generated from yesterday ####
Top_five_internal_cs <- cs %>% 
  filter(DaysAgo > -8) %>%
  # Take out internal ones
  filter(secondary_issue_type == "internally generated") %>% 
  group_by(typeName) %>% 
  dplyr::summarise(count=n()) %>% 
  arrange(-count) %>% 
  filter(typeName != "Miscellaneous" & typeName != "Welcome desk information" & typeName != "CS-Lost call (wrong #, hang up, dead air)" & typeName != "Report Man")


# If more than 5, mow it down
if(nrow(Top_five_internal_cs) > 5 ){
  Top_five_internal_cs <- Top_five_internal_cs[1:5,]
}


# Arrange them for the charts
Top_five_internal_cs <- arrange(Top_five_internal_cs, count)




############## Web Analytics ##############

ga.df <- read.csv("//fileshare1/Departments/Somerstat/Common/Data/2015_City_Web_Analytics/raw_data/LastTwentyFour.csv")

TopFifteen_ga <- ga.df %>%
  filter(pageTitle != "City of Somerville, Massachusetts" & 
           pageTitle != "Departments |" &
           pageTitle != "Search |" &
           pageTitle != "Contact Us |") %>%
  arrange(-pageviews)


TopFifteen_ga <- TopFifteen_ga[1:15,]

TopFifteen_ga <- arrange(TopFifteen_ga, pageviews)




############## Citizenserve ##############

isd <- read.csv("//fileshare1/Departments2/Somerstat Data/Inspectional_Services/data/Daily_Permits.csv")

# More dates
isd$Date <- as.Date(isd$IssueDate, "%m/%d/%Y")
isd <- DateVariableMaker(isd, "Date")


## Top from last day ## 
Top_isd <- isd %>%
  filter(DaysAgo > -2) %>%
  group_by(PermitType) %>%
  dplyr::summarize(count=n()) %>%
  arrange(-count)

# If more than 5, mow it down
if(nrow(Top_isd) > 5 ){
  Top_isd <- Top_isd[1:5,]
}

Top_isd <- arrange(Top_isd, count)


## Map it
forMap_isd <- isd %>%
  filter(DaysAgo > -8, Latitude != 0) %>%
  select(Latitude, Longitude, ProjectName, PermitAmount, Address, PermitTypeDetail, PermitType) %>% 
  mutate(PermitAmount = as.numeric(PermitAmount),
         Latitude = round(Latitude, 5),
         Longitude = round(Longitude, 5),
         # Charcs messing up geojson
         ProjectName = gsub("\"","", ProjectName),
         ProjectName = gsub("\t", " ", ProjectName),
         ProjectName = gsub('"', '', ProjectName))


# Convert to geojson and put it on our server
toGeoJSON(forMap_isd, "BuildingPermits", "./tmp/")

ftpUpload(what = "./tmp/BuildingPermits.geojson",
          to = paste(Somerville_server, "BuildingPermits.geojson", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/geo/daily/")




############## Knit ##############

## Now knit together the data and HTML in highcharts and save it to the server ##
library(knitr)

knit("./test-daily.Rhtml", output = "./tmp/test-daily.html")

# Upload to the daily dashboard
ftpUpload(what = "./tmp/daily.html",
          to = paste(Somerville_server, "test-daily.html", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/")



