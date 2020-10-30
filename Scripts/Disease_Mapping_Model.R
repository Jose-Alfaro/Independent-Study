library(ggplot2)
library(shapefiles)
library(sp)
library(CARBayes)
library(leaflet)
library(rgdal)
library(leaflet)
library(shiny)
library(shinycssloaders)
library(spdep)
library(GGally)
library(coda)
library(vcd)
library(MASS)

## Loads count data from Github directly (Previously dta)
count <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
names(count) <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2",
                  "Province_State", "Country_Region", "lat", "long", "Combined_key",
                  as.character(as.Date(12:ncol(count) - 12, origin = "2020/01/22")))
count <- subset(count, !is.na(FIPS))
count$FIPS <- sprintf("%05d", count$FIPS)
excl <- c("Alaska", "Hawaii", "American Samoa","Diamond Princess", "Grand Princess", "Guam", "Northern Mariana Islands", 
          "Puerto Rico", "Virgin Islands")
count <- subset(count, !Province_State %in% excl & lat > 0)
count$UID <- count$iso2 <- count$iso3 <- count$code3 <- count$Country_Region <- count$Combined_key <- NULL
count[,-(1:5)] <- t(apply(count[,-(1:5)], 1, function(x) pmax(0, diff(c(0, x)))))
row.names(count) <- count$FIPS

## Loads deaths data from Github directly
death <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))
names(death) <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2",
                  "Province_State", "Country_Region", "lat", "long", "Combined_key", "Population",
                  as.character(as.Date(13:ncol(death) - 13, origin = "2020/01/22")))
death <- subset(death, !is.na(FIPS))
death$FIPS <- sprintf("%05d", death$FIPS)
death <- subset(death, !Province_State %in% excl & lat > 0)
death$UID <- death$iso2 <- death$iso3 <- death$code3 <- death$Country_Region <- death$Combined_key <- NULL
death[,-(1:6)] <- t(apply(death[,-(1:6)], 1, function(x) pmax(0, diff(c(0, x)))))
row.names(death) <- death$FIPS

## Loads unemployment/median income dataset
income <- read.csv("../Data/Unemployment.csv")
income <- income[, c(1, 87)]
colnames(income) <- c("FIPS", "Income")
income$FIPS <- sprintf("%05d", income$FIPS)
income$Income <- as.numeric(gsub(",","",income$Income))

## Loads County Political Affiliation Based on 2016 Election
political <- read.csv("../Data/2016_US_County_Level_Presidential_Results.csv")
colnames(political)[11] <- "FIPS"
political$FIPS <- sprintf("%05d", political$FIPS)
political$Party <- as.factor(ifelse(political$per_dem > political$per_gop, 1, 0))
political <- political[, c(11, 12)]

## Date Specification Function
selectdates <- function(data = death, start, end){
  # Calculates Death Sums
  keep <- data[, 1:6]
  data <- data[, -c(1:6)]
  tmp1 <- as.Date(names(data))
  tmp2 <- which(tmp1 >= as.Date(start) & tmp1 <= as.Date(end))
  tmp <- data[, tmp2]
  Death_Sum <- rowSums(tmp)
  
  # Calculate Count_Sum and Perc_Sum
  cdata <- count[, -c(1:5)]
  ctmp1 <- as.Date(names(cdata))
  ctmp2 <- which(ctmp1 >= as.Date(start) & ctmp1 <= as.Date(end))
  ctmp <- cdata[, ctmp2]
  Count_Sum <- rowSums(ctmp)
  Perc_Sum <- rowSums((ctmp[, 1:ncol(ctmp)]/keep[, 6]) * 100)
  
  # # Mortality Rate by County
  # tmp_Death_Sum <- rowSums(data)
  # Mortality_Rate <- tmp_Death_Sum/keep[, 6]
  # Expected_Death <- Mortality_Rate * keep[, 6]
  # SMR <- Death_Sum/Expected_Death
  # 
  # # Incidence Rate by County
  # tmp_count_sum <- rowSums(cdata)
  # Incidence_Rate <- tmp_count_sum/keep[, 6]
  # Expected_Count <- Incidence_Rate * keep[, 6]
  # SIR <- Count_Sum/Expected_Count
  # 
  # Combines Datasets  
  tmp <- cbind(keep, Count_Sum, Perc_Sum, Death_Sum)
  tmp <- merge(tmp, income, by = "FIPS")
  tmp <- merge(tmp, political, by = "FIPS")
  tmp <- tmp[!(tmp$FIPS == 25007 | tmp$FIPS == 25019 | tmp$FIPS == 53055), ]
  tmp$logcount <- log(1 + tmp$Count_Sum)
  # # Calculates Expected Counts via Model
  # tmp$logcount <- log(tmp$Count_Sum + 1)
  # model1 <- lm(logcount ~ Population + Death_Sum + Income, data = tmp)
  # tmp$Expected_Count <- ceiling(exp(model1$fitted.values))
  # tmp$SIR <- tmp$Count_Sum/tmp$Expected_Count
  # 
  # # Calculates Expected Death via Model
  # tmp$logdeath <- log(tmp$Death_Sum + 1)
  # model2 <- lm(logdeath ~ Population + Count_Sum + Income, data = tmp)
  # tmp$Expected_Death <- ceiling(exp(model2$fitted.values))
  # tmp$SMR <- tmp$Death_Sum/tmp$Expected_Death
  # 
  row.names(tmp) <- tmp$FIPS
  return(tmp)
}

## Sample Dataset
full <- selectdates(start = "2020-09-01", end = "2020-09-30")
full <- full[, c(1, 2, 4, 5, 6, 7, 9, 10, 11, 12)]

## Loads SHP and DBF File
covidshp <- read.shp("../Shape_Files/cb_2018_us_county_500k.shp")
coviddbf <- read.dbf("../Shape_Files/cb_2018_us_county_500k.dbf")            
coviddbf$dbf <- data.frame(FIBS = with(coviddbf$dbf, paste0(STATEFP, COUNTYFP)), coviddbf$dbf)
covidshpstate <- read.shp("../Shape_Files/cb_2018_us_state_500k.shp")
coviddbfstate <- read.dbf("../Shape_Files/cb_2018_us_state_500k.dbf")   

## Default shape file
## Confirmed cases given dates
covid.sp <- combine.data.shapefile(
    data = full,
    shp = covidshp, dbf = coviddbf)
proj4string(covid.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
covid.sp <- spTransform(covid.sp, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# ## Format popup data for leaflet map.
# pop_num <- prettyNum(covid.sp$SIR, big.mark = ',', preserve.width = "none")
# popup_dat <- paste0("<strong>County: </strong>", 
#                     covid.sp$Admin2, 
#                     "<br><strong>Value: </strong>", 
#                     pop_num)
labels <- sprintf(
  "<strong>%s</strong><br/>%g Cases",
  covid.sp$Admin2, covid.sp$Count_Sum
) %>% lapply(htmltools::HTML)

colours <- colorNumeric(palette = "YlOrRd", domain = covid.sp@data$Count_Sum)
map0 <-  leaflet(data = covid.sp) %>%
  addTiles() %>% 
  addPolygons(
    layerId = ~FIPS,
    fillColor = ~ colours(covid.sp@data$Count_Sum),
    weight = 1,
    opacity = 0.7,
    color = "white",
    dashArray = '3',
    fillOpacity = 0.7,
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE)) %>%
  addLegend(
    pal = colours,
    values = covid.sp@data$Count_Sum,
    opacity = 1,
    title = "CaseCounts") %>%
  addScaleBar(position = "bottomleft")

## Linear regression model 
## Non-Transformed
model <- lm(Count_Sum ~ Population + Death_Sum + Income, data = full)
par(mfrow = c(2,2))
plot(model)

## Log Transformed
model <- lm(logcount ~ Population + Death_Sum , data = full)
par(mfrow = c(2,2))
plot(model)

ggpairs(data = full, c(9, 4, 6, 7, 8))

## Test for Autocorrelation Using Moran's I statistic
## Null Hypothesis: No Spatial Autocorrelation
W.nb <- poly2nb(covid.sp, row.names = rownames(covid.sp@data))
W.list <- nb2listw(W.nb, style = "B")
moran.mc(x = residuals(model), listw = W.list, nsim = 1000)

## Spatial Modeling

## Create the Neighborhood Matrix W
W <- nb2mat(W.nb, style="B")

## Creates Dissimilarity Matrix
income <- covid.sp@data$Income

Z <- lapply(c("Population", "Party"), function(x)
    as.matrix(dist(full[,x], diag = TRUE, upper = TRUE)))
           
chain1 <- S.CARdissimilarity(formula = floor(sqrt(Count_Sum)) ~ 1,
                             data = covid.sp@data,
                             family = "poisson", W = W, Z = Z, 
                             W.binary = TRUE, burnin = 2000, n.sample = 10000, thin = 2)

chain2 <- S.CARdissimilarity(formula = floor(logcount) ~ 1,
                             data = covid.sp@data,
                             family = "poisson", W = W, Z = Z, 
                             W.binary = TRUE, burnin = 2000, n.sample = 10000, thin = 2)

## Check to see if MC have convereged
## Method 1: Traceplots
beta.samples <- mcmc.list(chain2$samples$beta)
plot(beta.samples) # Plots for 3 covariates

## Note: This model represents the log risk surface with only an intercept term and random effects
print(chain2)

## Number and locations of boundaries
border.locations <- chain2$localised.structure$W.posterior

## Computes SIR
covid.sp@data$risk <- chain2$fitted.values
covid.sp@data$fitted_counts <- chain2$fitted.values

boundary.final <- highlight.borders(border.locations = border.locations,
                                    spdata = covid.sp)

colours <- colorNumeric(palette = "YlOrRd", domain = covid.sp@data$risk)

map3 <- leaflet(data = covid.sp) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(risk), color = "", weight = 1,
              fillOpacity = 0.7) %>%
  addLegend(pal = colours, values = covid.sp@data$risk, opacity = 1,
            title = "Fitted Vlaues") %>%
  addCircles(lng = ~boundary.final$X, lat = ~boundary.final$Y, weight = 1,
             radius = 2) %>%
  addScaleBar(position = "bottomleft")
map3
