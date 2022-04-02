library(ggplot2)
library(shapefiles)
library(sp)
library(CARBayes)
library(leaflet)
library(rgdal)
library(leaflet)
library(shiny)

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

## Data set up for county population
perc <- read.csv("../Data/PopulationEstimates.csv")
perc <- subset(perc, select = c(1, 20))
colnames(perc) <- c("FIPS", "Pop_Estimate_2019") 
perc$FIPS <- sprintf("%05d", perc$FIPS)
perc$Pop_Estimate_2019 <- as.numeric(gsub(",", "", perc$Pop_Estimate_2019))
perc <- merge(perc, tmp, by = "FIPS") ## dim 3108 255
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


## Define UI for application that draws a histogram
ui <- fluidPage(    
    ## Application title
    titlePanel("United States COVID-19 Mapping"),
    tags$em("By: Jose Alfaro"),
    tags$hr(),
    ## Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("daterange", "Date Range:",
                           start = as.character(Sys.Date() - 6),
                           end = as.character(Sys.Date())),
            selectInput("ptChoice", "Type of Plot:", choices = c("", "County-Wise", "State-Wise")),
            selectInput("typeChoice", "Data Type:", choices = c("", "Raw", "Percentage")),
            actionButton("submitButton", "Submit", class = "btn btn-primary")
        ),
        ## Display leaflet plot of cases
        mainPanel(
            leafletOutput("countyPlot"),
            leafletOutput("statePlot")
        )
    )
)

## Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$ptChoice, {
        req(input$ptchoice)
        if(input$ptChoice == "County-Wide"){
            hide("statePlot")
            show("countyPlot")
        }
        else{
            hide("countyPlot")
            show("statePlot")
        }
    })
    fdta <- eventReactive(input$typeChoice, {
        if (input$typeChoice == "Raw"){
            df <- selectdates(data = tmp, start = input$daterange[1], end = input$daterange[2])
        } else if (input$typeChoice == "Percentage"){
            df <- selectdates(data = perc, start = input$daterange[1], end = input$daterange[2])
        } else {return(NULL)}
        row.names(df) <- df$FIPS
        df
    })
    
    observeEvent(input$submitButton, {
        output$statePlot <- renderLeaflet({
            ## Instead of FIPS, this shape file uses STATEFP?            
        })
        output$countyPlot <- renderLeaflet({
            ## Confirmed cases given dates
            covid.sp <- combine.data.shapefile(data = fdta(), shp = covidshp, dbf = coviddbf)
            proj4string(covid.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
            covid.sp <- spTransform(covid.sp, CRS("+proj=longlat +datum=WGS84 +no_defs"))            
            ## Format popup data for leaflet map.
            pop_num <- prettyNum(covid.sp$Sum, big.mark = ',', preserve.width = "none")
            popup_dat <- paste0("<strong>County: </strong>", 
                                covid.sp$Admin2, 
                                "<br><strong>Value: </strong>", 
                                pop_num)
            colours <- colorNumeric(palette = "YlOrRd", domain = covid.sp@data$Sum)
            leaflet(covid.sp) %>%
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
        })
    })
}

## Run the application 
shinyApp(ui = ui, server = server)
