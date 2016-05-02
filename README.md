# Somerville_Daily_Dashboard
Analyzes data and makes dynamic charts, graphs, and maps for http://www.somervillema.gov/dashboard/daily.html

## Idea
Basically, this crunches the data and 'knits' it into an HTML file using knitr. The HTML relies on highcharts for the charts and leaflet for the maps. Also, it uploads the HTML and geojson files to our production server. 

For the theory and another working example, see http://danielphadley.com/How-To-Dashboard-R/

## How to automate 
1. In Windows task scheduler, I create a new task that runs daily with highest privilges
2. The "Actions" is "start a program," which points to the .bat file in this directory
3. The .bat file runs the .R script, which uploads the data to our web server through FTP

## TODO
+ ~~Refactor and clean code~~
+ ~~Make sure all NAs in charts get replaced with 0s~~
+ ~~Basic daily maps~~
+ ~~Tracking for QOL calls~~
+ ~~Make tracking weekly, rather than monthly~~
+ Replace rats and other QOL with actionable
+ Find a better viz for the temperature
+ ~~Make tracking based on some measure of statistical significance~~
+ ~~Try to do charts by 7 day periods, or at least label the x-axis by max(date)~~  
+ Seperate DPW driven from work requests
+ ~~Top three increases map and chart from top 25~~
+ ~~Map of top building permits~~
+ News articles from Python package
+ Links to top new website anomalies
+ T&P
+ Crime
+ Call forecasting - new feature
+ ~~Weather~~
## Actionable data:
+ Average time to completion vs. This week: DPW
+ Percent completed average vs this week DPW
+ Largest change in ratio of on-time as gauge graph
+ Largest change in ratio of open to close
+ Histogram for selected work order
