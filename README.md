# Somerville_Daily_Dashboard
Analyzes data and makes dynamic charts, graphs, and maps for http://www.somervillema.gov/dashboard/daily.html

## Idea
Basically, this crunches the data and 'knits' it into an HTML file using knitr. The HTML relies on highcharts for the charts and leaflet for the maps. Also, it uploads the HTML and geojson files to our production server 

For the theory and another working example, see http://danielphadley.com/How-To-Dashboard-R/

## How to automate 
1. In Windows task scheduler, I create a new task that runs daily with highest privilges
2. The "Actions" is "start a program," which points to the .bat file in this directory
3. The .bat file runs the .R script, which uploads the data to our web server through FTP

## How to transfer to another machine
1. Use github to get the latest version, which should be current on the official Somerville account
2. Change the file paths in all R scripts and .bat files to reflect your desktop environment
3. Copy the config.R file from the current maintainer (this is not commited in Github)
4. Install all of the R packages listed in each script
5. Follow the instruction above to automate 
6. Profit 

# TODO
+ ~~Refactor and clean code~~
+ ~~Make sure all NAs in charts get replaced with 0s~~
+ ~~Basic daily maps~~
+ ~~Tracking for QOL calls~~
+ ~~Make tracking weekly, rather than monthly~~
+ ~~Top internally-generated work orders~~
+ ~~Scrub every comment of weird characters before turning to geojson~~
+ ~~Make tracking based on some measure of statistical significance~~
+ ~~Try to do charts by 7 day periods, or at least label the x-axis by max(date)~~  
+ ~~Seperate DPW driven from work requests~~
+ ~~Top three increases map and chart from top 25~~
+ ~~Map of top building permits~~
+ ~~Weather~~


