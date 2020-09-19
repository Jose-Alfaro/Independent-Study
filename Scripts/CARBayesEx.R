## Example 1

library(CARBayesdata)
library(shapefiles)
library(sp)
data(lipdata)
data(lipdbf)
data(lipshp)

library(CARBayes)
lipdbf$dbf <- lipdbf$dbf[ ,c(2,1)]
data.combined <- combine.data.shapefile(data=lipdata, shp=lipshp, dbf=lipdbf)

## Example 2

library(CARBayesdata)
library(sp)
data(GGHB.IG)
data(pricedata)
head(pricedata)

library(dplyr)
pricedata <- pricedata %>% mutate(logprice = log(pricedata$price))

library(GGally)
ggpairs(data = pricedata, columns = c(8, 3:7))
pricedata.sp <- merge(x=GGHB.IG, y=pricedata, by="IG", all.x=FALSE)

library(rgdal)
pricedata.sp <- spTransform(pricedata.sp, CRS("+proj=longlat +datum=WGS84 +no_defs"))

library(leaflet)
colours <- colorNumeric(palette = "YlOrRd", domain = pricedata.sp@data$price)
map1 <- leaflet(data = pricedata.sp) %>%
    addTiles() %>%
    addPolygons(fillColor = ~colours(price), color="", weight=1,
                fillOpacity = 0.7) %>%
    addLegend(pal = colours, values = pricedata.sp@data$price, opacity = 1,
              title="Price") %>%
    addScaleBar(position="bottomleft")
map1


leaflet(data = pricedata.sp) %>% addTiles() %>%
    addPolygons(fillColor = ~colours(price), color="", weight=1, fillOpacity = 0.7)
