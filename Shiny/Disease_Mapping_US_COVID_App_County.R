#devtools::install_github("rstudio/leaflet")
#devtools::install_github("edwindj/leaflet")
## https://github.com/rstudio/leaflet/issues/496

library(ggplot2)
library(shapefiles)
library(sp)
library(CARBayes)
library(leaflet)
library(rgdal)
library(leaflet)
library(shiny)
library(shinycssloaders)

## Load data from Github directly and prepare `tmp`
dta <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
names(dta) <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2",
                "Province_State", "Country_Region", "lat", "long", "Combined_key",
                as.character(as.Date(12:ncol(dta) - 12, origin = "2020/01/22")))
dta <- subset(dta, !is.na(FIPS))
dta$FIPS <- sprintf("%05d", dta$FIPS)
excl <- c("Alaska", "Hawaii", "American Samoa","Diamond Princess", "Grand Princess", "Guam", "Northern Mariana Islands", 
          "Puerto Rico", "Virgin Islands")
dta <- subset(dta, !Province_State %in% excl & lat > 0)
dta$UID <- dta$iso2 <- dta$iso3 <- dta$code3 <- dta$Country_Region <- dta$Combined_key <- NULL
dta[,-(1:5)] <- t(apply(dta[,-(1:5)], 1, function(x) pmax(0, diff(c(0, x)))))
row.names(dta) <- dta$FIPS

## Data set up for county population
perc <- read.csv("../Data/PopulationEstimates.csv")
perc <- subset(perc, select = c(1, 20))
colnames(perc) <- c("FIPS", "Pop_Estimate_2019") 
perc$FIPS <- sprintf("%05d", perc$FIPS)
perc$Pop_Estimate_2019 <- as.numeric(gsub(",", "", perc$Pop_Estimate_2019))
perc <- merge(perc, dta, by = "FIPS") ## dim 3108 255
perc[, 7:ncol(perc)] <- perc[, 7:ncol(perc)]/perc[, 2] * 100
perc$Pop_Estimate_2019 <- NULL
row.names(perc) <- perc$FIPS 

## Loads SHP and DBF File
covidshp <- read.shp("../Shape_Files/cb_2018_us_county_500k.shp")
coviddbf <- read.dbf("../Shape_Files/cb_2018_us_county_500k.dbf")            
coviddbf$dbf <- data.frame(FIBS = with(coviddbf$dbf, paste0(STATEFP, COUNTYFP)), coviddbf$dbf)
covidshpstate <- read.shp("../Shape_Files/cb_2018_us_state_500k.shp")
coviddbfstate <- read.dbf("../Shape_Files/cb_2018_us_state_500k.dbf")            

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

## Default shape file
## Confirmed cases given dates
covid.sp <- combine.data.shapefile(
  data = selectdates(data = dta, start = Sys.Date() - 6, end = Sys.Date()),
  shp = covidshp, dbf = coviddbf)
proj4string(covid.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
covid.sp <- spTransform(covid.sp, CRS("+proj=longlat +datum=WGS84 +no_defs"))
## Format popup data for leaflet map.
pop_num <- prettyNum(covid.sp$Sum, big.mark = ',', preserve.width = "none")
popup_dat <- paste0("<strong>County: </strong>", 
                    covid.sp$Admin2, 
                    "<br><strong>Value: </strong>", 
                    pop_num)
colours <- colorNumeric(palette = "YlOrRd", covid.sp@data$Sum)

## Options for loader
options(spinner.color = "#0275D8", spinner.color.background = "white", spinner.size = 2)

map0 <-  leaflet(data = covid.sp) %>%
  addTiles() %>% 
  addPolygons(
    layerId = ~FIPS,
    fillColor = ~ colours(Sum),
    weight = 1,
    opacity = 0.7,
    color = "white",
    dashArray = '3',
    fillOpacity = 0.7,
    popup = popup_dat,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE)) %>%
  addLegend(
    pal = colours,
    values = covid.sp@data$Sum,
    opacity = 1,
    title = "Count") %>%
  addScaleBar(position = "bottomleft")

ui <- fluidPage(    
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
  ),

  
  ## Application title
  titlePanel("United States COVID-19 Mapping - County level"),
  tags$em("By: Jose Alfaro"),
  tags$hr(),
  ## Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterange", "Date Range:",
                     start = as.character(Sys.Date() - 6),
                     end = as.character(Sys.Date())),
      checkboxInput("checkBox", "Select all dates", FALSE),
      textOutput("dateCheck"),
      selectInput("typeChoice", "Data Type:", choices = c("Raw", "Percentage")),
      actionButton("submitButton", "Submit", class = "btn btn-primary")
    ),
    ## Display leaflet plot of cases
    mainPanel(
      withSpinner(leafletOutput("casemap"), type = 4)
    )
  )
)

server <- function(input, output) {
  
  output$dateCheck <- renderText({
    validate(
      need(input$daterange[2] > input$daterange[1], "WARNING: End date is earlier than start date.")
    )
    validate(
      need(input$daterange[1] > as.Date("2020-01-21"), "WARNING: Start date is earlier than available data.")
    )
    validate(
      need(input$daterange[2] < as.character(Sys.Date() + 1), "WARNING: End date is later than available data.")
    )
  })
  
  output$casemap <- renderLeaflet(map0)    
  observeEvent(input$submitButton, {
    if (input$typeChoice == "Raw"){
      df <- selectdates(data = dta, start = input$daterange[1], end = input$daterange[2])
    } else if (input$typeChoice == "Percentage"){
      df <- selectdates(data = perc, start = input$daterange[1], end = input$daterange[2])
    } else {return(NULL)}
    row.names(df) <- df$FIPS
    new.covid.sp <- covid.sp
    new.covid.sp@data$Sum <- df$Sum
    new.colours <- colorNumeric(palette = "YlOrRd", domain = new.covid.sp@data$Sum)
    leafletProxy("casemap") %>% clearControls()
    leafletProxy("casemap", data = new.covid.sp) %>%
      setShapeStyle(
        layerId = ~FIPS,
        fillColor = ~ new.colours(new.covid.sp@data$Sum)) %>%
      addLegend(
        pal = new.colours,
        values = new.covid.sp@data$Sum,
        opacity = 1,
        title = "Count")        
  })
}

## Run the application 
shinyApp(ui = ui, server = server)
