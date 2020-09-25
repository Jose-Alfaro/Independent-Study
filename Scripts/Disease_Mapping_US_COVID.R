#' Suggestions on coding style:
#' 1. Use ## or #' for comments
#' 2. Don't use `setwd()` and assume the current working directory is where this script is at
#' 3. Don't use symbols like "(", ")", "/", "\" in naming files/folders
#' 4. Specify all packages in the begining
#' 5. Use ggplot2 family when possible
#' 6. Don't name files with date; we will use git to track changes.
#' If you really want to track changes, create a `changeLog` file.
#'
#' Comments:
#' 1. I removed everything that depends on `dplyr` because CARBayes seems to have a function called
#' `select` and I am not sure if this is causing your codes to fail.
#' 2. The Fips in week_count is numeric and so is the row.names. I re-defined these.
#' 3. Your 'data.combined' is already a SpatialPolygonsDataFrame; no need to merge.
#' Example 2's `GGHB.IG` don't have information in `pricedata` and that's the reason for merge.
#' 4. CRS is NA because we created the `SpatialPolygonsDataFrame` without definding it, so I
#' defined it with `proj4string()`, then I project it again with `spTransform()` to something
#' the leaflet can recognize.
#' 5. I think leaflet requires "lat" and "long" to specify coordinates (?);
#' stated on its online document but I am not sure if this is true. I changed the names anyways.
#'
#' To-do
#' 1. A function to allow user to specify which week or which month or any arbitrary window of date
#' 2. HighlightOption: I want to highlight a county when the mouse hovers over it so I used the
#' highlightOptions. But the border stays on even after mouse out. Can you fix it?
#' 3. Focus on 48 states?

## Required packages
library(ggplot2)
library(shapefiles)
library(sp)
library(CARBayes)
library(leaflet)
library(rgdal)
library(leaflet)
library(openair)

## Load data from Github directly and prepare `tmp`
dta <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
names(dta) <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2",
                "Province_State", "Country_Region", "lat", "long", "Combined_key",
                as.character(as.Date(12:ncol(dta) - 12, origin = "2020/01/22")))
dta <- subset(dta, !is.na(FIPS))
dta$FIPS <- sprintf("%05d", dta$FIPS)
excl <- c("Alaska", "Hawaii", "American Samoa","Diamond Princess", "Grand Princess", "Guam", "Northern Mariana Islands", 
          "Puerto Rico", "Virgin Islands")
tmp <- subset(dta, !Province_State %in% excl & lat > 0)
tmp$UID <- tmp$iso2 <- tmp$iso3 <- tmp$code3 <- tmp$Country_Region <- tmp$Combined_key <- NULL
row.names(tmp) <- tmp$FIPS


## Creates weekly sum column; the most recent 7 days
week_count <- subset(tmp, select = c(FIPS, Admin2, Province_State, lat, long))
week_count$week_sum <- rowSums(tmp[,tail(1:ncol(tmp), 7)])
row.names(week_count) <- week_count$FIPS
qplot(long, lat, data = week_count, 
      main = "Observations Locations (United States)", xlab = "Longitude", ylab = "Latitude")

## Creates monthly sum column; the most recent 30 days
month_count <- subset(tmp, select = c(Admin2, lat, long))
month_count$month_sum <- rowSums(tmp[,tail(1:ncol(tmp), 30)])
qplot(long, lat, data = month_count, 
      main = "Observations Locations (United States)", xlab = "Longitude", ylab = "Latitude")

## Weekly Sum in Texas
tx_weekly <- subset(week_count, Province_State == "Texas")
qplot(long, lat, data = tx_weekly,
      main = "Observations Locations (Texas)", xlab = "Longitude", ylab = "Latitude")


## ###################################################################################
## Combining data frame and shapefile to form SpatialPolygonsDataFrame
## This is to mimic example 2 (Section 5.1) in CARBayes vignette
## ###################################################################################

## Loads SHP and DBF File
covidshp <- read.shp("../Shape_Files/cb_2018_us_county_500k.shp")
coviddbf <- read.dbf("../Shape_Files/cb_2018_us_county_500k.dbf")
## coviddbf <- foreign::read.dbf("../US-Shape/US-Census-Shape/cb_2018_us_county_500k.dbf",
##                               as.is = TRUE)
coviddbf$dbf <- data.frame(FIBS = with(coviddbf$dbf, paste0(STATEFP, COUNTYFP)), coviddbf$dbf)

## Confirmed cases in the last 7 days
covid.sp <- combine.data.shapefile(data = week_count, shp = covidshp, dbf = coviddbf)
proj4string(covid.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
covid.sp <- spTransform(covid.sp, CRS("+proj=longlat +datum=WGS84 +no_defs"))

## Format popup data for leaflet map.
pop_num <- prettyNum(covid.sp$week_sum, big.mark = ',', preserve.width = "none")
popup_dat <- paste0("<strong>County: </strong>", 
                    covid.sp$Admin2, 
                    "<br><strong>Value: </strong>", 
                    pop_num)

colours <- colorNumeric(palette = "YlOrRd", domain = covid.sp@data$week_sum)
map1 <- leaflet(covid.sp) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~ colours(week_sum),
    weight = 1,
    opacity = 0.7,
    color = "white",
    dashArray = '3',
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    popup = popup_dat
  ) %>%
  addLegend(
    pal = colours,
    values = covid.sp@data$week_sum,
    opacity = 1,
    title = "Count"
  ) %>%
  addScaleBar(position = "bottomleft")
map1

## Confirmed cases in the last 30 days
covid.sp <- combine.data.shapefile(data = month_count, shp = covidshp, dbf = coviddbf)
proj4string(covid.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
covid.sp <- spTransform(covid.sp, CRS("+proj=longlat +datum=WGS84 +no_defs"))

## Format popup data for leaflet map.
pop_num <- prettyNum(covid.sp$month_sum, big.mark = ',', preserve.width = "none")
popup_dat <- paste0("<strong>County: </strong>", 
                    covid.sp$Admin2, 
                    "<br><strong>Value: </strong>", 
                    pop_num)

colours <- colorNumeric(palette = "YlOrRd", domain = covid.sp@data$month_sum)

map2 <- leaflet(covid.sp) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~ colours(month_sum),
    weight = 1,
    opacity = 0.7,
    color = "white",
    dashArray = '3',
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    popup = popup_dat
  ) %>%
  addLegend(
    pal = colours,
    values = covid.sp@data$month_sum,
    opacity = 1,
    title = "Count"
  ) %>%
  addScaleBar(position = "bottomleft")
map2



## Select by Date
selectdates <- function(data, start, end){
  tmp1 <- data[, 1:5]
  tmp2 <- data[, grep(start, colnames(data)) : grep(end, colnames(data))]
  tmp <- cbind(tmp1, tmp2)
  return(tmp)
} 

selectdates(data = tmp, start = "2020-09-16", end = "2020-09-22")

# OR

## Select by Date (Second Function)
selectdates2 <- function(data, start, end){
  keep <- data[, 1:5]
  data <- data[, -c(1:5)]
  tmp1 <- as.Date(names(data))
  tmp2 <- which(tmp1 >= as.Date(start) & tmp1 <= as.Date(end))
  tmp <- data[, tmp2]
  tmp$Sum <- rowSums(tmp)
  tmp <- cbind(keep, tmp)
  return(tmp)
}

selectdates2(data = tmp, start = "2020-09-16", end = "2020-09-22")
