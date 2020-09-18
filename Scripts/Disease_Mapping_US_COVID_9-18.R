library(dplyr)

### NOTE: Dataset is missing Wisconsin and Wyoming
setwd("C:/Users/josea/Desktop/Independent Study/Data")
dta <- read.csv("confirmed(9-18).csv", header = T)
dta <- na.omit(dta)
colnames(dta)[10] <- "Long"
names(dta)[12:251] <- substring(names(dta)[12:251], 2)
tmp <- dta %>% 
  select(5, 6, 7, 9, 10, 12:251) %>%
  filter(!Province_State %in% c("American Samoa","Diamond Princess", "Grand Princess", 
                                "Guam", "Northern Mariana Islands", 
                                "Puerto Rico", "Virgin Islands") & Lat != 0)
row.names(tmp) <- tmp$FIPS
## Creates weekly sum column
week_count <- tmp %>% mutate(week_sum = rowSums(.[238:244])) %>% select(FIPS, Admin2, Province_State, Lat, Long, week_sum) 
row.names(week_count) <- week_count$FIPS
plot(week_count$Long, week_count$Lat, main = "Observations Locations (United States)", xlab = "Longitude", ylab = "Latitude")


## Creates monthly sum column
month_count <- tmp %>% mutate(month_sum = rowSums(.[228:244])) %>% select(Lat, Long, month_sum)
plot(month_count$Long, month_count$Lat, main = "Observations Locations (United States)", xlab = "Longitude", ylab = "Latitude")

## Weekly Sum in Texas
tx_weekly <- week_count %>% filter(Province_State == "Texas")
plot(tx_weekly$Long, tx_weekly$Lat, main = "Observations Locations (Texas)", xlab = "Longitude", ylab = "Latitude")
colors <- colorNumeric(palette = "Y10rRd", domain = tx_weekly$week_sum)
# map1 <- leaflet(data = tx_weekly) %>% 
#   addTiles() %>%
#   addPolygons(fillColor = ~colors(week_sum), color = "", weight = 1,
#               fillOpacity = 0.7) %>%
#   addLegend(pal = colors, values = week_count, opacity = 1,
#             title = "Weekly Count") %>%
#   addScaleBar(position = "bottomleft")



## Combining data frame and shapefile to form SpatialPolygonsDataFrame
library(shapefiles)
library(sp)
library(CARBayes)

# Loads SHP and DBF File
setwd("C:/Users/josea/Desktop/Independent Study/Shape_Files")
covidshp <- read.shp("cb_2018_us_county_500k.shp")
coviddbf <- read.dbf("cb_2018_us_county_500k.dbf")

# Created FIBS column in DBF file
coviddbf$dbf$FIBS <- paste0(coviddbf$dbf$STATEFP, coviddbf$dbf$COUNTYFP)
coviddbf$dbf <- subset(coviddbf$dbf, select = c(10, 1:9))

# Created SpatialPolygonDataFrame
data.combined <- combine.data.shapefile(data = week_count, shp = covidshp, dbf = coviddbf)

## dim(week_count) = 3142 6
## dim(coviddbf$dbf) = 3233 10
## dim(data.combined) = 2826 6

covid.sp <- merge(x = data.combined, y = week_count, by = "FIPS", all.x = F)
pricedata.sp <- spTransform(covid.sp ,
                            + CRS("+proj=longlat +datum=WGS84 +no_defs"))

library(leaflet)
colours <- colorNumeric(palette = "YlOrRd", domain = covid.sp@data$week_sum.x)
### Error in UseMethod("rescale") :
###   no applicable method for 'rescale' applied to an object of class "data.frame"
library(leaflet)
map1 <- leaflet(data = data.combined) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(week_sum), color="", weight=1,
                fillOpacity = 0.7) %>%
  addLegend(pal = colours, values = data.combined$week_sum, opacity = 1,
              title="Price") %>%
  addScaleBar(position="bottomleft")
map1


## Trial 2: ggplot
library(ggplot2)

