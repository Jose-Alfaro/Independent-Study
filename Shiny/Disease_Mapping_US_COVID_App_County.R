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
library(tidyr)
library(tidyverse)

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

## Function for Top 5 Counties with Highest Deaths/Counts
animateTop <- function(start = Sys.Date() - 6, end = Sys.Date()){
  # Top Counts
  countdata <- count[, -c(1:5)]
  keep <- count[, 1:2]
  tmp1 <- as.Date(names(countdata))
  tmp2 <- which(tmp1 >= as.Date(start) & tmp1 <= as.Date(end))
  tmp <- countdata[, tmp2]
  Case_Count <- rowSums(tmp)
  
  Top_Case_Count <- head(sort(Case_Count, decreasing = T))
  counttmp <- cbind(keep, tmp)
  final_case_count <- counttmp[counttmp$FIPS == names(Top_Case_Count[1]) | counttmp$FIPS == names(Top_Case_Count[2]) | counttmp$FIPS == names(Top_Case_Count[3]) | counttmp$FIPS == names(Top_Case_Count[4]) | counttmp$FIPS == names(Top_Case_Count[5]),]
  final_case_count <- final_case_count[, -1]
  final_case_count <- as.data.frame(pivot_longer(final_case_count, cols = -Admin2, 
                                                 names_to = "Date"))
  final_case_count$Date <- as.Date(final_case_count$Date)
  
  # Top Deaths
  deathdata <- death[, -c(1:6)]
  deathkeep <- death[, 1:2]
  dtmp <- deathdata[, tmp2]
  Death_Count <- rowSums(dtmp)
  
  Top_Death_Count <- head(sort(Death_Count, decreasing = T))
  deathtmp <- cbind(deathkeep, dtmp)
  final_death_count <- deathtmp[deathtmp$FIPS == names(Top_Death_Count[1]) | deathtmp$FIPS == names(Top_Death_Count[2]) | deathtmp$FIPS == names(Top_Death_Count[3]) | deathtmp$FIPS == names(Top_Death_Count[4]) | deathtmp$FIPS == names(Top_Death_Count[5]),]
  final_death_count <- final_death_count[, -1]
  final_death_count <- as.data.frame(pivot_longer(final_death_count, cols = -Admin2, 
                                                  names_to = "Date"))
  final_death_count$Date <- as.Date(final_death_count$Date)
  
  return(list(cases = final_case_count, deaths = final_death_count))
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

## Creates Initial Animation
tempanimatedf <- animateDates(start = Sys.Date() - 6, end = Sys.Date()) 
tp <- ggplot(data = tempanimatedf, aes(x = Date)) +
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

gganimate::anim_save("outfile.gif", gganimate::animate(tp, fps = 5, nframes = 60))

## Bar Animation
bardta <- count[, -c(1, 3:5)]
bardta <- pivot_longer(bardta, cols = -Admin2, 
                       names_to = c("year", "month", "day"), names_sep = "-")
bardta <- transform(bardta, YearMonth = paste0(year, month))
bardta <- bardta[, -c(2:4)]
bardta <- aggregate(value ~ Admin2 + YearMonth, data=bardta, FUN=sum)

bar_formatted <- bardta %>%
  dplyr::group_by(YearMonth) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e9))) %>%
  dplyr::group_by(Admin2) %>% 
  filter(rank <=10) %>%
  ungroup()

staticplot <- ggplot(bar_formatted, aes(rank, group = Admin2, 
                                       fill = as.factor(Admin2), color = as.factor(Admin2))) +
  # coord_flip() + 
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Admin2, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = value,label = value, hjust=0)) + 
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim <- staticplot + transition_states(YearMonth, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'COVID-19 Cases by Month: {closest_state}',  
       subtitle  =  "Top 10 Counties",
       caption  = "COVID-19 Cases in the US | Data Source: Johns Hopkins University")

suppressWarnings(print(gganimate::anim_save("baranimation.gif", gganimate::animate(anim, fps = 5, nframes = 60))))

remove <- c("Alaska", "Hawaii")
state.names <- state.name[!state.name %in% remove]

## Initial Case Animation
animdta <- animateCases(start = "2020-01-22", end = Sys.Date())

staticplot2 <- ggplot(animdta$cases, aes(x = Date, y = value, group = Admin2, colour = Admin2)) +
  geom_line(lwd = 1.05) +
  scale_colour_discrete(guide = 'none') +
  scale_x_date(expand = c(0.1, 0)) +
  scale_y_continuous("Confirmed Cases") +
  geom_dl(aes(label = Admin2), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(aes(label = Admin2), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  ggtitle("Counties With Most Cases") +
  transition_reveal(Date)

gganimate::anim_save("caseanimation.gif", gganimate::animate(staticplot2, fps = 5, nframes = 60))

## Initial Death Animation
staticplot3 <- ggplot(animdta$deaths, aes(x = Date, y = value, group = Admin2, colour = Admin2)) +
  geom_line(lwd = 1.05) +
  scale_colour_discrete(guide = 'none') +
  scale_x_date(expand = c(0.1, 0)) +
  scale_y_continuous("Deaths") +
  geom_dl(aes(label = Admin2), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(aes(label = Admin2), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  ggtitle("Counties With Most Deaths") +
  transition_reveal(Date)

gganimate::anim_save("deathanimation.gif", gganimate::animate(staticplot3, fps = 5, nframes = 60))

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
    tabPanel("County Level", fluid = TRUE,
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
                 withSpinner(tableOutput('table')),
                 withSpinner(imageOutput("caseAnimation")),
                 withSpinner(imageOutput('deathAnimation'))
               )
             )
    ),
    ## Animation Pannel
    tabPanel("Animations", fluid = TRUE,
             sidebarLayout(fluid = TRUE,
               sidebarPanel(
                 selectInput("stateChoiceAnimation", "Select State:", choices = state.names), 
                 dateRangeInput("daterangeAnimation", "Date Range:",
                                start = as.character(Sys.Date() - 6),
                                end = as.character(Sys.Date()),
                                min = "2020-01-22",
                                max = Sys.Date()),
                 checkboxInput("checkBoxAnimation", "Select all dates", FALSE),
                 textOutput("dateCheckAnimation"),
                 actionButton("submitButtonAnimation", "Submit", class = "btn btn-primary")
               ),
               mainPanel(
                 splitLayout(
                   withSpinner(imageOutput("animation")),
                   withSpinner(imageOutput("barAnimation"))
                )
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
  output$animation <- renderImage({
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
    )
  }, deleteFile = TRUE)
  
  output$barAnimation <- renderImage({
    
    list(src = "baranimation.gif",
         contentType = 'image/gif'
    )
  }, deleteFile = TRUE)
  
  output$caseAnimation <- renderImage({
    
    list(src = "caseanimation.gif",
         contentType = 'image/gif'
    )
  }, deleteFile = TRUE)
  
  output$deathAnimation <- renderImage({
    
    list(src = "deathanimation.gif",
         contentType = 'image/gif'
    )
  }, deleteFile = TRUE)
  
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
      
      
      gganimate::anim_save("outfile.gif", gganimate::animate(p, fps = 5, nframes = 60)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
      )
    }, deleteFile = TRUE)
    
  })  
}

## Run the application 
shinyApp(ui = ui, server = server)