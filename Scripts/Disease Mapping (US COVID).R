library(dplyr)
library(leaflet)

### NOTE: Dataset is missing Wisconsin and Wyoming
setwd("C:/Users/josea/Desktop/Independent Study")
dta <- read.csv("confirmed (9-11).csv", header = T)
colnames(dta)[10] <- "Long"
names(dta)[12:244] <- substring(names(dta)[12:244], 2)
tmp <- dta %>% 
  select(6, 7, 9, 10, 12:244) %>%
  filter(!Province_State %in% c("American Samoa","Diamond Princess", "Grand Princess", 
                                "Guam", "Northern Mariana Islands", 
                                "Puerto Rico", "Virgin Islands") & Lat != 0)


## Creates weekly sum column
week_count <- tmp %>% mutate(week_sum = rowSums(.[231:237])) %>% select(Admin2, Province_State, Lat, Long, week_sum) 
plot(week_count$Long, week_count$Lat, main = "Observations Locations (United States)", xlab = "Longitude", ylab = "Latitude")


## Creates monthly sum column
month_count <- tmp %>% mutate(month_sum = rowSums(.[228:237])) %>% select(Lat, Long, month_sum)


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


