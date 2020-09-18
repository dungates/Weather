library(rnoaa)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Shows types of data available
ncdc_datasets()

# Function to find latest daily precipitation data based on city
cityweatherData <- function(city) {
  # Fetches list of cities
  var <- ncdc_locs(locationcategoryid = c('CITY'), sortfield = 'name', sortorder = 'asc', limit = 1000)$data
  citySelected <- var %>% dplyr::filter(grepl(as.character(city), name))
  # Fetches station id list with temperature data
  var2 <- ncdc_stations(datasetid='GHCND', locationid=citySelected$id, datatypeid = 'dly-tmax-normal')$data
  # Gets mindate and maxdate of station
  stationID = var2 %>% dplyr::filter(datacoverage == max(datacoverage))
  # Gets one year of preciptation data cause apparently thats the max you can get
  cityDailyPrecip <- ncdc(datasetid = 'GHCND', stationid = stationID$id, datatypeid = 'PRCP',
                          startdate = (ymd(stationID$maxdate) - years(1)), 
                          enddate = ymd(stationID$maxdate), limit = 1000, add_units = T)$data
  cityDailyPrecip <- as.data.frame(cityDailyPrecip) %>% arrange(desc(date))
  precipGraph <- cityDailyPrecip %>% ggplot(aes(x = ymd_hms(date), y = value*0.03937)) + geom_line() +
    ggtitle(paste("Daily Precipation of Last Year in", city)) + labs(x = "Date", y = "Inches") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()
  # Gets one year of temperature data
  
  #cityDailyTemp <- something
  assign("cityDailyPrecip", cityDailyPrecip, envir = .GlobalEnv)
  tibble::view(cityDailyPrecip)
  precipGraph
}

cityweatherData("Portland")

# Make a plot with geom_density_ridges once you can get temperature data


# Where is temperature data??
out <- ncdc(datasetid='NORMAL_DLY', stationid='GHCND:USW00004236', datatypeid='dly-tmax-normal', 
            startdate = '2019-01-01', enddate = '2019-12-11', limit = 300, add_units = T)$data
ncdc_stations(datasetid='NORMAL_DLY', locationid=citySelected$id, datatypeid = 'dly-tmax-normal')$data
cityDailyTemp <- ncdc(datasetid = 'NORMAL_DLY', datatypeid = 'PRCP', stationid = stationID$id, 
     startdate = (ymd(stationID$maxdate) - years(1)), 
     enddate = ymd(stationID$maxdate), limit = 1000, add_units = T)$data
