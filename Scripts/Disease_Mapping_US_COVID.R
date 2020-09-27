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
row.names(tmp) <- tmp$FIPS ## dim(tmp) 3108 254

## Data set up for county population
county <- read.csv("../Data/PopulationEstimates.csv")
county <- subset(county, select = c(1, 20))
colnames(county) <- c("FIPS", "Pop_Estimate_2019") 
county$FIPS <- sprintf("%05d", county$FIPS)
county$Pop_Estimate_2019 <- as.numeric(gsub(",", "", county$Pop_Estimate_2019))
county <- merge(county, tmp, by = "FIPS") ## dim 3108 255
county[, 7:ncol(county)] <- county[, 7:ncol(county)]/county[, 2] * 100
county$Pop_Estimate_2019 <- NULL
row.names(county) <- county$FIPS 

## Date Specification Function
selectdates <- function(data, start, end){
  keep <- data[, 1:5]
  data <- data[, -c(1:5)]
  tmp1 <- as.Date(names(data))
  tmp2 <- which(tmp1 >= as.Date(start) & tmp1 <= as.Date(end))
  tmp <- data[, tmp2]
  Sum <- rowSums(tmp)
  tmp <- cbind(keep, Sum)
  return(tmp)
}

county <- selectdates(data = county, start = "2020-09-20", end = "2020-09-26")

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

## Loads SHP and DBF File
covidshp <- read.shp("../Shape_Files/cb_2018_us_county_500k.shp")
coviddbf <- read.dbf("../Shape_Files/cb_2018_us_county_500k.dbf")

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


## Confirmed cases in the last 7 days (Adjusted by County)
covid.sp <- combine.data.shapefile(data = county, shp = covidshp, dbf = coviddbf)
proj4string(covid.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
covid.sp <- spTransform(covid.sp, CRS("+proj=longlat +datum=WGS84 +no_defs"))

## Format popup data for leaflet map.
pop_num <- prettyNum(covid.sp$Sum, big.mark = ',', preserve.width = "none")
popup_dat <- paste0("<strong>County: </strong>", 
                    covid.sp$Admin2, 
                    "<br><strong>Value: </strong>", 
                    pop_num)

colours <- colorNumeric(palette = "YlOrRd", domain = covid.sp@data$Sum)
map1 <- leaflet(covid.sp) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~ colours(Sum),
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
    values = covid.sp@data$Sum,
    opacity = 1,
    title = "Count"
  ) %>%
  addScaleBar(position = "bottomleft")
map1



## Aggregate By State
state_count <- selectdates(data = tmp, start = "2020-09-16", end = "2020-09-22")
state_count <- data.frame(STATEFP = substr(state_count$FIPS, 0, 2), state_count[,c(3,6)])
state_count <- aggregate(state_count$Sum, by = list(state_count$STATEFP), FUN = sum)
colnames(state_count) <- c("STATEFP", "Count")
row.names(state_count) <- state_count$STATEFP



## State-Wide Confirmed Cases 
coviddbf <- read.dbf("../Shape_Files/cb_2018_us_county_500k.dbf")
coviddbf$dbf$STATEFP <- as.character(coviddbf$dbf$STATEFP)
covid.sp <- combine.data.shapefile(data = state_count, shp = covidshp, dbf = coviddbf)
## WARNINGS happen here
proj4string(covid.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
covid.sp <- spTransform(covid.sp, CRS("+proj=longlat +datum=WGS84 +no_defs"))