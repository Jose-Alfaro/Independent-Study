## devtools::install_github("rstudio/leaflet")
## devtools::install_github("edwindj/leaflet")
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
library(DT)
library(gganimate)
library(ggthemes)

setwd("C:/Users/josea/Desktop/Independent Study/Shiny App")
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
  
  # Uses count dataset to calculate Count_Sum and Perc_Sum
  cdata <- count[, -c(1:5)]
  ctmp1 <- as.Date(names(cdata))
  ctmp2 <- which(ctmp1 >= as.Date(start) & ctmp1 <= as.Date(end))
  ctmp <- cdata[, ctmp2]
  
  Count_Sum <- rowSums(ctmp)
  Perc_Sum <- rowSums((ctmp[, 1:ncol(ctmp)]/keep[, 6]) * 100)
  
  tmp <- cbind(keep, Count_Sum, Perc_Sum, Death_Sum)
  tmp$Population <- NULL
  row.names(tmp) <- tmp$FIPS
  return(tmp)
}

## Calculates Daily Sum for Aimation
animateDates <- function(data = death, start, end){
  # Calculates Daily Death Counts
  data <- data[, -c(1:6)]
  tmp1 <- as.Date(names(data))
  tmp2 <- which(tmp1 >= as.Date(start) & tmp1 <= as.Date(end))
  tmp <- data[, tmp2]
  Daily_Death <- colSums(tmp)
  
  # Calcualtes Daily Counts
  cdata <- count[, -c(1:5)]
  ctmp1 <- as.Date(names(cdata))
  ctmp2 <- which(ctmp1 >= as.Date(start) & ctmp1 <= as.Date(end))
  ctmp <- cdata[, ctmp2]
  Daily_Count <- colSums(ctmp)
  
  tmp <- cbind(Daily_Count, Daily_Death)
  tmp <- data.frame(Date = row.names(tmp), tmp)
  rownames(tmp) <- NULL
  tmp$Date <- as.Date(tmp$Date)
  return(tmp)
}

## Loads SHP and DBF File
covidshp <- read.shp("../Shape_Files/cb_2018_us_county_500k.shp")
coviddbf <- read.dbf("../Shape_Files/cb_2018_us_county_500k.dbf")            
coviddbf$dbf <- data.frame(FIBS = with(coviddbf$dbf, paste0(STATEFP, COUNTYFP)), coviddbf$dbf)
covidshpstate <- read.shp("../Shape_Files/cb_2018_us_state_500k.shp")
coviddbfstate <- read.dbf("../Shape_Files/cb_2018_us_state_500k.dbf")   

## Default shape file
## Confirmed cases given dates
covid.sp <- combine.data.shapefile(
  data = selectdates(start = Sys.Date() - 6, end = Sys.Date()),
  shp = covidshp, dbf = coviddbf)
proj4string(covid.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
covid.sp <- spTransform(covid.sp, CRS("+proj=longlat +datum=WGS84 +no_defs"))

## Format popup data for leaflet map.
pop_num <- prettyNum(covid.sp$Count_Sum, big.mark = ',', preserve.width = "none")
popup_dat <- paste0("<strong>County: </strong>", 
                    covid.sp$Admin2, 
                    "<br><strong>Value: </strong>", 
                    pop_num)
colours <- colorNumeric(palette = "YlOrRd", covid.sp@data$Count_Sum)

## Options for loader
options(spinner.color = "#0275D8", spinner.color.background = "white", spinner.size = 2)

labels <- sprintf(
  "<strong>%s</strong><br/>%g Cases",
  covid.sp$Admin2, covid.sp$Count_Sum
) %>% lapply(htmltools::HTML)

map0 <-  leaflet(data = covid.sp) %>%
  addTiles() %>% 
  addPolygons(
    layerId = ~FIPS,
    fillColor = ~ colours(Count_Sum),
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
      bringToFront = TRUE), label = labels, labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
  addLegend(
    pal = colours,
    values = covid.sp@data$Count_Sum,
    opacity = 1,
    title = "Count") %>%
  addScaleBar(position = "bottomleft")

## Creates Initial Table
tbl_dta <- covid.sp@data
colnames(tbl_dta) <- c("FIPS", "County", "State", "Latitude", "Longitude",
                       "Case Count", "Percent Sum", "Death Count")
tbl_dta <- tbl_dta[, -c(1, 4, 5)]
tbl_dta$`Case Count` <- as.integer(tbl_dta$`Case Count`)
tbl_dta$`Death Count` <- as.integer(tbl_dta$`Death Count`)
table0 <- head(tbl_dta[order(tbl_dta$`Case Count`, decreasing = TRUE),], 10)

## UI Function Begins
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
  titlePanel("United States COVID-19 Mapping - County Level"),
  tags$em("By: Jose Alfaro"),
  tags$hr(),
  
  ## Map Panel
  tabsetPanel(
    tabPanel("Map", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("daterange", "Date Range:",
                                start = as.character(Sys.Date() - 6),
                                end = as.character(Sys.Date()),
                                min = "2020-01-22",
                                max = Sys.Date()),
                 checkboxInput("checkBox", "Select all dates", FALSE),
                 textOutput("dateCheck"),
                 selectInput("typeChoice", "Data Type:", choices = c("Raw", "Percentage")),
                 actionButton("submitButton", "Submit", class = "btn btn-primary")
               ),
               mainPanel(
                 withSpinner(leafletOutput("casemap"), type = 4),
                 withSpinner(tableOutput('table'))
               )
             )
    ),
    
    ## Animation Pannel
    tabPanel("Animation", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
               ),
               mainPanel(
                 withSpinner(imageOutput("animation"))
               )
             )
    )
  )
)

## Server Function Begins
server <- function(input, output, session) {
  DF1 <- reactiveValues(data = NULL)
  observe({
    DF1$data <- table0
    if (input$checkBox == TRUE){
      updateDateRangeInput(session,
                           "daterange",
                           "Date Range:",
                           start = "2020-01-22",
                           end = Sys.Date(),
                           min = "2020-01-22",
                           max = Sys.Date())
    }
    if((input$checkBox == T & input$daterange[1] != "2020-01-22") | (input$checkBox == T & input$daterange[2] != Sys.Date())){
      updateCheckboxInput(
        session =  session,
        inputId =  "checkBox", 
        value = FALSE
      )
    }
  })
  
  
  output$dateCheck <- renderText({
    validate(
      need(input$daterange[2] > input$daterange[1], "WARNING: End date is earlier than start date.")
    )
    validate(
      need(input$daterange[2] < as.character(Sys.Date() + 1), "WARNING: End date is later than available data.")
    )
  })
  
  ## Displays Initial Map and Table
  output$casemap <- renderLeaflet(map0)
  output$table <- renderTable(DF1$data)
  
  observeEvent(input$submitButton, {
    if (input$typeChoice == "Raw"){
      df <- selectdates(start = input$daterange[1], end = input$daterange[2])
      animatedf <- animateDates(start = input$daterange[1], end = input$daterange[2])
      df$Total <- df$Count_Sum
    } else if (input$typeChoice == "Percentage"){
      df <- selectdates(start = input$daterange[1], end = input$daterange[2])
      animatedf <- animateDates(start = input$daterange[1], end = input$daterange[2])
      df$Total <- df$Perc_Sum
    } else {return(NULL)}
    
    row.names(df) <- df$FIPS
    new.covid.sp <- covid.sp
    new.covid.sp@data$Total <- df$Total
    new.colours <- colorNumeric(palette = "YlOrRd", domain = new.covid.sp@data$Total)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Cases",
      new.covid.sp$Admin2, new.covid.sp$Count_Sum
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("casemap") %>% clearControls() %>% clearMarkers()
    leafletProxy("casemap", data = new.covid.sp) %>%
      setShapeStyle(
        layerId = ~FIPS,
        fillColor = ~ new.colours(new.covid.sp@data$Total)) %>%
      addLegend(
        pal = new.colours,
        values = new.covid.sp@data$Total,
        opacity = 1,
        title = "Count")
    
    colnames(df) <- c("FIPS", "County", "State", "Latitude", "Longitude",
                      "Case Count", "Percent Sum", "Death Count", "Variable of Interest")
    
    df <- df[, -c(1, 4, 5, 9)]
    df$`Case Count` <- as.integer(df$`Case Count`)
    df$`Death Count` <- as.integer(df$`Death Count`)
    
    DF1$data <- head(df[order(df$`Case Count`, decreasing = TRUE),], 10)
    
    output$animation <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      # now make the animation
      p <- ggplot(data = animatedf, aes(x = Date)) +
        geom_line(aes(y = Daily_Count, color = "Count"), size = 1.25, show.legend = T) +
        geom_line(aes(y = Daily_Death, color = "Death"), size = 1.25) +
        ggtitle("COVID-19 Cases and Deaths in US") +
        scale_x_date(date_labels = "%b %d") +
        ylab("Count") +
        scale_color_manual(values = c('Count' = 'steelblue', 'Death' = 'darkred')) +
        labs(color = "") +
        theme_bw() +
        theme(legend.position = "bottom") + 
        transition_reveal(Date)
      
      
      gganimate::anim_save("outfile.gif", gganimate::animate(p)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )
    }, deleteFile = TRUE)
    
  })  
}

## Run the application 
shinyApp(ui = ui, server = server)