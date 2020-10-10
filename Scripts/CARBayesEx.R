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

## Example 3

library(CARBayesdata)
library(sp)
data(GGHB.IG)
data(respiratorydata)
head(respiratorydata)

respiratorydata.sp <- merge(x = GGHB.IG, y = respiratorydata, by = "IG", all.x = FALSE)
respiratorydata.sp <- spTransform(respiratorydata.sp,
                                  CRS("+proj=longlat +datum=WGS84 +no_defs"))

library(leaflet)
colours <- colorNumeric(palette = "YlOrRd", domain = respiratorydata.sp@data$SMR)
map2 <- leaflet(data=respiratorydata.sp) %>%
    addTiles() %>%
    addPolygons(fillColor = ~colours(SMR), color="", weight=1,
                fillOpacity = 0.7) %>%
    addLegend(pal = colours, values = respiratorydata.sp@data$SMR, opacity = 1,
              title="SMR") %>%
    addScaleBar(position="bottomleft")
map2

library(spdep)
W.nb <- poly2nb(respiratorydata.sp, row.names = rownames(respiratorydata.sp@data))
W <- nb2mat(W.nb, style="B")


income <- respiratorydata.sp@data$incomedep
Z.incomedep <- as.matrix(dist(income, diag=TRUE, upper=TRUE))

library(CARBayes)
formula <- observed ~ offset(log(expected))
chain1 <- S.CARdissimilarity(formula=formula, data=respiratorydata.sp@data,
                             family="poisson", W=W, Z=list(Z.incomedep=Z.incomedep),
                             W.binary=TRUE, burnin=1000, n.sample=3000, thin=1)
print(chain1)

border.locations <- chain1$localised.structure$W.posterior
respiratorydata.sp@data$risk <- chain1$fitted.values / respiratorydata.sp@data$expected
boundary.final <- highlight.borders(border.locations=border.locations, spdata=respiratorydata.sp)

colours <- colorNumeric(palette = "YlOrRd", domain = respiratorydata.sp@data$risk)
map3 <- leaflet(data=respiratorydata.sp) %>%
    addTiles() %>%
    addPolygons(fillColor = ~colours(risk), color="", weight=1,
                fillOpacity = 0.7) %>%
    addLegend(pal = colours, values = respiratorydata.sp@data$risk, opacity = 1,
              title="Risk") %>%
    addCircles(lng = ~boundary.final$X, lat = ~boundary.final$Y, weight = 1,
               radius = 2) %>%
    addScaleBar(position="bottomleft")
map3

