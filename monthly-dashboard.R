############## Intro ############### 

## Created 6/16 by Daniel Hadley to load and visualize data from various sources ##
# and make charts for somervillema.gov/dashboard/monthly.html 

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




############## Police ############### 

## Read Data and make new Variables ##
ci <- read.csv("//fileshare1/Departments2/Somerstat Data/Police/daily/data_pipeline_dont_use/CriminalIncidents.csv")
qol <- read.csv("//fileshare1/Departments2/Somerstat Data/Police/daily/data_pipeline_dont_use/QualityOfLife.csv")


# Clean
ci$DateTime <- parse_date_time(ci$dtreported, orders = "%m-%d-%y %I:%M:%S %p")
ci$Date <- format(ci$DateTime, '%Y-%m-%d')
ci <- add_date_vars(ci, "Date")

qol$DateTime <- parse_date_time(qol$dtreceived, orders = "%m-%d-%y %I:%M:%S %p")
qol$Date <- format(qol$DateTime, '%Y-%m-%d')
qol <- add_date_vars(qol, "Date")

qol$inctype <- tolower(qol$inctype)



### CI ###
## Time series
TimeSeries_ci <- make_x_day_ts_multiple_v(ci, "Date", 30.5, "offense")

# Get it down to about 12 months and format a couple things
TimeSeries_ci <- TimeSeries_ci %>% 
  tail(13) %>% 
  mutate(period_ending = format(period_ending, format = "%b %d")) %>% 
  data.frame() # the other format was throwing off printing



## CI map
forMap_ci_monthly <- ci %>% 
  filter(days_ago > -31 & X != "" & X != 0) %>% 
  select(Y, X, offense, Date) %>% 
  rename(latitude = Y, longitude = X)

# Convert and upload to our server
toGeoJSON(forMap_ci_monthly, "PoliceCI_monthly", "./tmp/")

ftpUpload(what = "./tmp/PoliceCI_monthly.geojson",
          to = paste(Somerville_server, "PoliceCI_monthly.geojson", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/geo/monthly/")




### Significant QOL ###
# Now run it through my function that sorts by growth
qol_increases <- sort_by_ts_statistical_growth(qol, "Date", x_days = 30.5, "inctype", n_threshold = 4)

# a vector with the top 3
unique_qol_top_three <- c(as.character(qol_increases$v_names[1]),
                         as.character(qol_increases$v_names[2]),
                         as.character(qol_increases$v_names[3]))

# We use this to build the gauges and maps too
TopThreeIncreases_qol_all <- qol %>% 
  filter(inctype %in% unique_qol_top_three) 

TopThreeIncreases_qol <- TopThreeIncreases_qol_all %>%
  select(Y, X, inctype, Date, days_ago) %>% 
  rename(latitude = Y, longitude = X)


# First the time series chart
# Make the ts using my function
forChart_TopThree_qol <- make_x_day_ts_multiple_v(TopThreeIncreases_qol_all, "Date", x_days = 30.5, "inctype")

# clean up
forChart_TopThree_qol <- forChart_TopThree_qol %>% 
  tail(13) %>% 
  mutate(Date_max = format(period_ending, format = "%b %d")) %>% 
  data.frame(check.names = FALSE) # the other format was throwing off printing 


# Then the Map
forMap_qol <- TopThreeIncreases_qol %>%
  mutate(latitude = as.numeric(as.character(latitude))) %>% 
  filter(days_ago > -31 & latitude != "") %>%
  filter(latitude != 0) %>% 
  select(-days_ago)


# Convert and upload to our server
toGeoJSON(forMap_qol, "PoliceQOL_monthly", "./tmp/")

ftpUpload(what = "./tmp/PoliceQOL_monthly.geojson",
          to = paste(Somerville_server, "PoliceQOL_monthly.geojson", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/geo/monthly/")





############## 311 ############### 

## Read Data and make new Variables ##
cs <- read.csv("//fileshare1/Departments2/Somerstat Data/Constituent_Services/data/data_pipeline_dont_use/311_Somerville.csv")


# returns string w/o leading or trailing whitespace
# I was having trouble with trailing spaces, e.g. "Graffiti "
# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
cs <- as.data.frame(apply(cs,2,function (cs) sub("\\s+$", "", cs)))

# don't know why this was stored as an integer
cs$typeName <- as.character(cs$typeName)

cs$Date <- as.Date(cs$displayDate, "%m/%d/%Y")
cs <- add_date_vars(cs, "Date")




#### Find statistical time-series anomalies & make a chart and map of top 3 ####

# Make geojson & chart for the Unique Top 3 map #
# Put it in a function so as to keep code clean

## First the algo to find which work orders had the largest (statistical) increase last week
# First get work orders
cs_work_requests <- cs %>% filter(secondary_issue_type == "Service Requests")

# Now run it through my function that sorts by growth
cs_increases <- sort_by_ts_statistical_growth(cs_work_requests, "Date", x_days = 30.5, "typeName", n_threshold = 10)

# a vector with the top 3
unique_cs_top_three <- c(as.character(cs_increases$v_names[1]),
                         as.character(cs_increases$v_names[2]),
                         as.character(cs_increases$v_names[3]))


# We use this to build the gauges and maps too
TopThreeIncreases_cs_all <- cs_work_requests %>% 
  filter(typeName %in% unique_cs_top_three) 

TopThreeIncreases_cs <- TopThreeIncreases_cs_all %>% 
  select(latitude, longitude, typeName, Date, days_ago, comments)


# First the time series chart
# Make the ts using my function
forChart_TopThree_cs <- make_x_day_ts_multiple_v(TopThreeIncreases_cs_all, "Date", x_days = 30.5, "typeName")

# clean up
forChart_TopThree_cs <- forChart_TopThree_cs %>% 
  tail(13) %>% 
  mutate(Date_max = format(period_ending, format = "%b %d")) %>% 
  data.frame(check.names = FALSE) # the other format was throwing off printing 


# Then the Map
forMap_cs <- TopThreeIncreases_cs %>%
  mutate(latitude = as.numeric(as.character(latitude))) %>% 
  filter(days_ago > -8 & latitude != "") %>%
  filter(latitude != 0) %>% 
  # Too many sensitive details in the comments
  select(-days_ago, -comments)





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




#### Top from last month ####
TopFifteen_cs <- cs %>% 
  filter(days_ago > -30) %>%
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




#### Last month ####

# to get the count of calls
last_month_cs <- cs %>%
  filter(secondary_issue_type != "internally generated") %>% 
  filter(days_ago > -31 & days_ago < 0)





#### Quality of Life ####
cs_qol <- cs %>% 
  filter(secondary_issue_type != "internally generated") %>% 
  filter(typeName == "Rats" | typeName == "Graffiti" | typeName == "Pothole") 


# Make the chart with my function
forTS_qol <- make_x_day_ts_multiple_v(cs_qol, "Date", 30, "typeName")

# clean up
forTS_qol <- forTS_qol %>% 
  tail(13) %>% 
  mutate(Date_max = format(period_ending, format = "%b %d")) %>% 
  data.frame(check.names = FALSE) # the other format was throwing off printing 



## Make geojson for the top quality-of-life calls ##
forMap_qol <- cs_qol %>% 
  filter(days_ago > -31) %>%
  select(latitude, longitude, typeName)




#### Convert to geojson and put it on our server ####

toGeoJSON(forMap_cs, "UniqueCS_monthly", "./tmp/")

toGeoJSON(forMap_qol, "QualityOfLifeCS_monthly", "./tmp/")

ftpUpload(what = "./tmp/UniqueCS_monthly.geojson",
          to = paste(Somerville_server, "UniqueCS_monthly.geojson", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/geo/monthly/")

ftpUpload(what = "./tmp/QualityOfLifeCS_monthly.geojson",
          to = paste(Somerville_server, "QualityOfLifeCS_monthly.geojson", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/geo/monthly/")




#### Top internally-generated from yesterday ####
cs_internal <- cs %>% 
  filter(secondary_issue_type == "internally generated")


Top_five_internal_cs <- cs_internal %>% 
  filter(days_ago > -31) %>%
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


## Get the ones with the largest statistical increase

# Quick function to format for the tables
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

top_three_increases_internal_cs <- sort_by_ts_statistical_growth(cs_internal, "Date", x_days = 30.5, n_threshold = 2, var_of_interest = "typeName")




############## Web Analytics ##############

ga.df <- read.csv("//fileshare1/Departments2/Somerstat Data/Website_Analytics/data_pipeline_dont_use/LastTwentyFour.csv")

TopFifteen_ga <- ga.df %>%
  filter(pageTitle != "City of Somerville, Massachusetts" & 
           pageTitle != "Departments |" &
           pageTitle != "Search |" &
           pageTitle != "Contact Us |") %>%
  arrange(-pageviews)


TopFifteen_ga <- TopFifteen_ga[1:15,]

TopFifteen_ga <- arrange(TopFifteen_ga, pageviews)




############## Citizenserve ##############

isd <- read.csv("//fileshare1/Departments2/Somerstat Data/Inspectional_Services/data/data_pipeline_dont_use/Daily_Permits.csv")

# More dates
isd$Date <- as.Date(isd$IssueDate, "%m/%d/%Y")
isd <- add_date_vars(isd, "Date")


## Top from last month ## 
Top_isd <- isd %>%
  filter(days_ago > -31) %>%
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
  filter(days_ago > -31, Latitude != 0) %>%
  select(Latitude, Longitude, ProjectName, PermitAmount, Address, PermitTypeDetail, PermitType) %>% 
  mutate(PermitAmount = as.numeric(PermitAmount),
         Latitude = round(Latitude, 5),
         Longitude = round(Longitude, 5),
         # Charcs messing up geojson
         ProjectName = gsub("\"","", ProjectName),
         ProjectName = gsub("\t", " ", ProjectName),
         ProjectName = gsub('"', '', ProjectName))


# Convert to geojson and put it on our server
toGeoJSON(forMap_isd, "BuildingPermits_monthly", "./tmp/")

ftpUpload(what = "./tmp/BuildingPermits.geojson",
          to = paste(Somerville_server, "BuildingPermits_monthly.geojson", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/geo/monthly/")




############## Knit ##############

## Now knit together the data and HTML in highcharts and save it to the server ##
# I first do a test for my testing environment, then full save
library(knitr)

knit("./monthly.Rhtml", output = "./tmp/test-monthly.html")

# Upload to the dashboard
ftpUpload(what = "./tmp/test-monthly.html",
          to = paste(Somerville_server, "test-monthly.html", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/")




knit("./monthly.Rhtml", output = "./tmp/monthly.html")

# Upload to the dashboard
ftpUpload(what = "./tmp/monthly.html",
          to = paste(Somerville_server, "monthly.html", sep = ""),
          verbose = TRUE,
          userpwd = Somerville_server_userpwd, 
          prequote="CWD /var/www/dashboard/")



