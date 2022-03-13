#libraries to include

library(shiny)
library(tuple)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(scales)
library(DT)
library(tidyr)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)

# assume all of the tsv files in this directory are data of the same kind that I want to visualize
entriesData <- do.call(rbind, lapply(list.files(pattern = "*Totals.tsv"), read.delim))
locData <- do.call(rbind, lapply(list.files(pattern = "*Stops.tsv"), read.delim))
Randolph <- data.frame(NA, NA, NA, NA, NA, 40200, NA, "false", "false", "true", "true", "true", "false", "false", "true", "true", NA, 41.884431,  -87.626149)
names(Randolph)<-c("STOP_ID", "DIRECTION_ID", "STOP_NAME", "STATION_NAME", "STATION_DESCRIPTIVE_NAME", "MAP_ID", "ADA", "RED", "BLUE", "G", "BRN", "P", "Pexp", "Y", "Pnk", "O", "Location", "lat", "long")
Madison <- data.frame(NA, NA, NA, NA, NA, 40640, NA, "false", "false", "true", "true", "true", "false", "false", "true", "true", NA, 41.882023, -87.626098)
names(Madison)<-c("STOP_ID", "DIRECTION_ID", "STOP_NAME", "STATION_NAME", "STATION_DESCRIPTIVE_NAME", "MAP_ID", "ADA", "RED", "BLUE", "G", "BRN", "P", "Pexp", "Y", "Pnk", "O", "Location", "lat", "long")
Washington <- data.frame(NA, NA, NA, NA, NA, 40500, NA, "true", "false", "false", "false", "false", "false", "false", "false", "false", NA, 41.8837, -87.6278)
names(Washington)<-c("STOP_ID", "DIRECTION_ID", "STOP_NAME", "STATION_NAME", "STATION_DESCRIPTIVE_NAME", "MAP_ID", "ADA", "RED", "BLUE", "G", "BRN", "P", "Pexp", "Y", "Pnk", "O", "Location", "lat", "long")
Homan <- data.frame(NA, NA, NA, NA, NA, 41580, NA, "false", "false", "true", "false", "false", "false", "false", "false", "false", NA, 41.884914, -87.711327)
names(Homan)<-c("STOP_ID", "DIRECTION_ID", "STOP_NAME", "STATION_NAME", "STATION_DESCRIPTIVE_NAME", "MAP_ID", "ADA", "RED", "BLUE", "G", "BRN", "P", "Pexp", "Y", "Pnk", "O", "Location", "lat", "long")
locData <- rbind(locData, Randolph, Madison, Washington, Homan)
# print(head(locData))
# class(locData)
# locData <- data.frame(locData)

entriesData$newDate <- as.Date(entriesData$date, "%m/%d/%Y")
entriesData$date <- NULL
entriesData$stationname[entriesData$stationname == "Skokie"] <- "Dempster-Skokie"

all_data_df <- entriesData

# transfer data from location file to main data frame
all_data_df$long <- locData$long[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$lat <- locData$lat[match(all_data_df$station_id, locData$MAP_ID)]

head(all_data_df)
# getting rid of the stations with no location info
all_data_df <- subset(all_data_df, !is.na(long))
# all_data_df %>% filter(station_id == 41580)

all_data_df$RedLine <- locData$RED[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$BlueLine <- locData$BLUE[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$GreenLine <- locData$G[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$BrownLine <- locData$BRN[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$PurpleLine <- locData$Pexp[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$YellowLine <- locData$Y[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$PinkLine <- locData$Pnk[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$OrangeLine <- locData$O[match(all_data_df$station_id, locData$MAP_ID)]

name <- all_data_df[!duplicated(all_data_df[,c('station_id')]),]

getStationName <- function(station) {
  toReturn <- 0
  for (i in 1:nrow(name)) {
    if (name[i,1] == station) {
      # print(name[i,2])
      toReturn <- name[i,2]
      break
    }
  }
  toReturn
}

red_s_names <- data.frame(stationname=c())
blue_s_names <- data.frame(stationname=c())
green_s_names <- data.frame(stationname=c())
brown_s_names <- data.frame(stationname=c())
orange_s_names <- data.frame(stationname=c())
purple_s_names <- data.frame(stationname=c())
yellow_s_names <- data.frame(stationname=c())
pink_s_names <- data.frame(stationname=c())

for (i in 1:nrow(locData)) {
  # print(getStationName(locData[i,6]))
  if(locData[i,14] == "true"){
    yellow_s_names <- rbind(yellow_s_names,getStationName(locData[i,6]))
  }
  if (locData[i,8] == "true") {
    #Red
    red_s_names <- rbind(red_s_names,getStationName(locData[i,6]))
  }

  if (locData[i,9] == "true") {
    #Blue
    blue_s_names <- rbind(blue_s_names,getStationName(locData[i,6]))
  }

  if (locData[i,10] == "true") {
    #Green
    green_s_names <- rbind(green_s_names,getStationName(locData[i,6]))
  }

  if (locData[i,11] == "true") {
    #Brown
    brown_s_names <- rbind(brown_s_names,getStationName(locData[i,6]))
  }

  if (locData[i,13] == "true") {
    #Purple
    purple_s_names <- rbind(purple_s_names,getStationName(locData[i,6]))
  }

  if (locData[i,15] == "true") {
    #Pink
    pink_s_names <- rbind(pink_s_names,getStationName(locData[i,6]))
  }

  if (locData[i,16] == "true") {
    #Orange
    orange_s_names <- rbind(orange_s_names,getStationName(locData[i,6]))
  }
}

red_s_names <- distinct(red_s_names)
colnames(red_s_names) <- "stationname"
blue_s_names <- distinct(blue_s_names)
colnames(blue_s_names) <- "stationname"
green_s_names <- distinct(green_s_names)
colnames(green_s_names) <- "stationname"
yellow_s_names <- distinct(yellow_s_names)
colnames(yellow_s_names) <- "stationname"
orange_s_names <- distinct(orange_s_names)
colnames(orange_s_names) <- "stationname"
brown_s_names <- distinct(brown_s_names)
colnames(brown_s_names) <- "stationname"
pink_s_names <- distinct(pink_s_names)
colnames(pink_s_names) <- "stationname"
purple_s_names <- distinct(purple_s_names)
colnames(purple_s_names) <- "stationname"

Red_df <- subset(all_data_df, all_data_df$RedLine == "true")
Blue_df <- subset(all_data_df, all_data_df$BlueLine == "true")
Green_df <- subset(all_data_df, all_data_df$GreenLine == "true")
Brown_df <- subset(all_data_df, all_data_df$BrownLine == "true")
Purple_df <- subset(all_data_df, all_data_df$PurpleLine == "true")
Yellow_df <- subset(all_data_df, all_data_df$YellowLine == "true")
Pink_df <- subset(all_data_df, all_data_df$PinkLine == "true")
Orange_df <- subset(all_data_df, all_data_df$OrangeLine == "true")


# red_s_names <- unique(Red_df[c("stationname")])






unique_all_data_df <- all_data_df[!duplicated(all_data_df[,c('stationname')]),]
unique_all_data_df <- unique_all_data_df[order(unique_all_data_df$stationname), ]

station_names <- unique(all_data_df[c("stationname")])
station_names <- station_names[order(station_names$stationname), ]

getMapID <- function(station) {
  toReturn <- 0
  for (i in 1:nrow(unique_all_data_df)) {
    if (unique_all_data_df[i,2] == station) {
      toReturn <- unique_all_data_df[i,1]
      break
    }
  }
  
  toReturn
}



# create data frame for a particular station
create_station_df <- function(station) {
  subset(all_data_df, all_data_df$stationname == station)
}

# get data frame for a particular station
get_station_df <- function(station) {
  station_df[[1]][which(station_names == station)]
}

# create a data frame for each station
station_df <- list(lapply(station_names, create_station_df))

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

# position to view all CTA stations on map
default_long <- -87.658753
default_lat <- 41.866568
default_zoom <- 11

# Create the shiny application
ui <- dashboardPage(
  
  
  
  dashboardHeader(title = "CTA Subway Data"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     id = "tabs",
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("All Stations", tabName = "viz", selected = T),
                     menuItem("One Station", tabName = "ovw"),
                     menuItem("About", tabName = "About")
                   ),
                   shinyjs::useShinyjs()
  ),
  dashboardBody(
    
    tabItems(
      tabItem(
        tags$head(tags$style(
          type = "text/css",
          "#controlPanel {background-color: rgba(255,255,255,0.8);}",
          ".leaflet-top.leaflet-right .leaflet-control {
      margin-top: 0px;
    }"
        )),
        tabName = "viz",
              
              sidebarLayout(
                position = "left",
                sidebarPanel(
                  style = "margin-top:100%; padding: 15px;",
                  checkboxInput("single_date_check",
                                label = "Single Date",
                                value = TRUE
                  ),
                  dateInput("single_date_input",
                            label = "Single Date",
                            value = "2021-08-23",
                            min = "2001-01-01",
                            max = "2021-11-30",
                            format = "m / dd / yyyy"
                  ),
                  checkboxInput("range_date_check",
                                label = "Ridership Difference between Two Dates",
                                value = FALSE
                  ),
                  dateInput("range_start_date_input",
                            label = "Date 1",
                            value = "2020-08-23",
                            min = "2001-01-01",
                            max = "2021-11-30",
                            format = "m / dd / yyyy"
                  ),
                  dateInput("range_end_date_input",
                            label = "Date 2",
                            value = "2021-08-23",
                            min = "2001-01-01",
                            max = "2021-11-30",
                            format = "m / dd / yyyy"
                  ),
                  selectInput("bar_chart_type",
                              "Sort Bar Chart",
                              choices = c("Alphabetical", "Ascending", "Descending"),
                              selected = c("Ascending")),
                  selectInput("stations",
                              label = "Stations",
                              choices = station_names,
                              selected = "UIC-Halsted"
                  ),
                  actionButton("prev_day_btn",
                               label = "Previous Day"
                  ),
                  actionButton("next_day_btn",
                               label = "Next Day"
                  ),
                  width = 2
                ),
                mainPanel(
                  tags$style(type = "text/css", "#mainMap {height: calc(45vh) !important;}"),
                  tags$style(type = "text/css", "#mainBarGraph {height: calc(85vh) !important;}"),
                  tags$style(HTML(' .leaflet-top { top: 50%; } ')),
                  tags$style(HTML('.btn-primary, .btn-primary:hover, .btn-primary:active, .btn-primary:visited {background-color: #8064A2 !important;}')),
                  
                  fluidRow(
                    column(7,
                           box(solidHeader = TRUE, status = "primary", width = 200,
                               div(plotOutput("mainBarGraph"))
                           )
                           
                    ),
                    column(5,
                           box(solidHeader = TRUE, status = "primary", width = 200,
                               uiOutput("mainTable"),
                           ),
                           box(id = "map_container",solidHeader = TRUE, status = "primary", width = 200,
                               p("Click on a station on the map to highlight it on the bar chart"),
                               leafletOutput("mainMap")
                           )
                    )
                  ),
                  width = 10
                )
              )
      ),
      tabItem(tabName = "ovw",
              tags$style(type = "text/css", "#mainMap2 {height: calc(45vh) !important;}"),
              # tags$style(HTML(' .leaflet-top { top: 50%; } ')),
              
              fluidRow(
                
                column(4,
                       br(),br(),br(),br(),
                       br(),br(),br(),br(),
                       br(),br(),br(),br(),
                       br(),
                       p("Choose line color", style = "padding-left: 20px;"),
                       column(6,
                              
                              column(3,
                                     style = "padding-left: 5px;",
                                     checkboxInput("red", "Red", value = TRUE),
                              ),
                              column(3,
                                     checkboxInput("blue", "Blue", value = TRUE),
                              ),
                              column(3,
                                     checkboxInput("green", "Green", value = TRUE),
                              ),
                              column(3,
                                     checkboxInput("brown", "Brown", value = TRUE),
                              ),
                       ),
                       column(6,
                              column(3,
                                     style = "padding-left: 0px;",
                                     checkboxInput("purple", "Purple", value = TRUE),
                              ),
                              column(3,
                                     checkboxInput("yellow", "Yellow", value = TRUE),
                              ),
                              column(3,
                                     checkboxInput("pink", "Pink", value = TRUE),
                              ),
                              column(3,
                                     checkboxInput("orange", "Orange", value = TRUE),
                              ),
                       ),
                       column(4,
                              p("Choose a station", style = "padding-left: 5px;"),
                              selectInput("Station", NULL,
                                          choices = station_names, 
                                          selected = "Clark/Lake"
                              )
                       ),
                       column(4,
                              p("Choose Chart 2 type"),
                              selectInput("Chart", NULL,
                                          choices = c("Daily", "Monthly", "Weekdays"), 
                                          selected = "Daily"
                              )
                       ),
                       column(4,
                              p("Choose year"),
                              selectInput("Year", NULL,
                                          choices = c(2001:2021), 
                                          selected = 2001
                              )
                       ),
                       p("Choose a station by clicking on it on the map", align = 'center'),
                       box(solidHeader = TRUE, status = "primary", width = 250,
                           leafletOutput("mainMap2")
                       )
                ),
                column(8,
                       fluidRow(
                         style = "padding: 15px;",
                         actionButton("RedLine", label = "Red Line", style = "color: #fff; background-color: #e92f2f;"),
                         actionButton("BlueLine", label = "Blue Line", style = "color: #fff; background-color: #3c95d6;"),
                         actionButton("GreenLine", label = "Green Line", style = "color: #fff; background-color: #359140;"),
                         actionButton("BrownLine", label = "Brown Line", style = "color: #fff; background-color: #964B00;"),
                         actionButton("PurpleLine", label = "Purple Line", style = "color: #fff; background-color: #482887;"),
                         actionButton("YellowLine", label = "Yellow Line", style = "color: #000; background-color: #f0e21b;"),
                         actionButton("PinkLine", label = "Pink Line", style = "color: #fff; background-color: #d57a9e;"),
                         actionButton("OrangeLine", label = "Orange Line", style = "color: #fff; background-color: #dd4b26;")
                       ),
                       
                       fluidRow(
                         br(),
                         column(6,
                                
                                box(solidHeader = TRUE, status = "primary", width = 200,
                                    h4("Chart 1", align = 'center'),
                                    plotOutput("Station_Yearly_Bar")
                                )
                         ),
                         column(6, 
                                box(solidHeader = TRUE, status = "primary", width = 200,
                                    h4("Chart 2", align = 'center'),
                                    plotOutput("Station_Bar")
                                )
                         ),
                         
                         fluidRow(
                           style = "padding: 0px;",
                           
                           column(6,
                                  
                                    
                                    
                           )
                           
                           
                         ),
                         
                         column(6,
                                box(solidHeader = TRUE, status = "primary", width = 200,
                                    uiOutput("Station_Yearly_Raw")
                                )
                                
                         ),
                         
                         column(6,
                                box(solidHeader = TRUE, status = "primary", width = 200,
                                    uiOutput("Station_Raw")
                                )
                         )
                       )
                )
              )
      ),
      
      #About page
      tabItem(tabName = "About",
              h2("About Page"),
              verbatimTextOutput("AboutOut")
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent({
    input$Station
  },
  
  {
    temp <- get_station_df(input$Station)
    temp <- do.call(rbind.data.frame, temp)
    temp <- temp %>%
      mutate(year = format(newDate, "%Y")) %>%
      group_by(year) %>%
      summarise(rides = sum(rides))
    
    val <- max(temp$year)
    print(val)
    print(input$Year)
    updateSelectInput(session, "Year",
                      choices = c(min(temp$year):max(temp$year)), 
                      selected = val
    )
    print(input$Year)
  })
  
  getBaseMap <- function() {
    # creating initial map
    m <- leaflet(options = leafletOptions(preferCanvas = T)) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(default_long, default_lat, default_zoom) %>%
      addResetMapButton() %>% # Add reset button
      addProviderTiles(providers$Stamen.TonerLite, group = "Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark", options = providerTileOptions(
        updateWhenZooming = F,
        updateWhenIdle = T
      )) %>%
      addLayersControl(
        baseGroups = c("Default", "Light", "Dark"),
        options = layersControlOptions(collapsed = FALSE)
      )
    m
  }
  
  
  
  addCirclesMarkers <- function() {
    m <- getBaseMap()
    
    # join ridership data of selected date range with location data
    df = sum_of_station_df_react_date()
    
    df['popUp'] <- "No ridernship data"
    
    dataToUse <- df
    
    for (i in 1:nrow(df))
    {
      # 16 is the index where popUp is
      dataToUse[i,17] <- paste0("Station Name: ", dataToUse[i,2], "<br>", "Entries: ", dataToUse[i,4])
    }
    
    pal <- colorNumeric("OrRd", dataToUse$rides)
    # print(pal(df$rides))
    
    h <- dataToUse %>% filter(toHighlight == "Yes")
    #print(h)
    
    m <- m %>% addCircleMarkers(data = dataToUse, ~long, ~lat,
                                popup = dataToUse$popUp,
                                weight = 3,
                                radius = 10,
                                color = "black",
                                fillColor = ~pal(rides),
                                stroke = T,
                                fillOpacity = 0.50,
                                layerId = dataToUse$stationname) %>%
      addLegend(position="topright", pal = pal, values = dataToUse$rides) %>% 
      addCircleMarkers(lng = h[1,]$long, lat = h[1,]$lat, color = 'blue')
      
    
    m
  }
  
  addCirclesMarkers2 <- function() {
    m <- getBaseMap()
    
    
    h <- name %>% filter(stationname == input$Station)
    
    m <- m %>% addCircleMarkers(data = name, ~long, ~lat,
                                popup = name$stationname,
                                weight = 3,
                                radius = 10,
                                color = "black",
                                fillColor = "#F0B79A",
                                stroke = T,
                                fillOpacity = 0.50,
                                layerId = name$station_id) %>%
      addCircleMarkers(lng = h[1,]$long, lat = h[1,]$lat, color = 'blue')
    
    #print(head(all_data_df))
    
    m
  }
  
  #################################################################
  
  # create a bar graph
  
  output$mainBarGraph <- renderPlot({
    
    df = sum_of_station_df_react_date_station()
    toReturn <- NULL
    
    
    if (input$bar_chart_type == "Alphabetical") {
      toReturn <- ggplot(data = df, aes(x = station_names, y = rides))
    }
    
    if (input$bar_chart_type == "Ascending") {
      toReturn <- ggplot(data = df, aes(x = reorder(station_names, rides), y = rides))
    }
    
    if (input$bar_chart_type == "Descending") {
      toReturn <- ggplot(data = df, aes(x = reorder(station_names, -rides), y = rides))
    }
    
    toReturn <- toReturn +
      scale_x_discrete(limits = rev) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
      coord_flip() +
      theme_dark()
    
    if (input$single_date_check) {
      # output$text <- renderText({ 
      #   paste("Date: ", input$single_date_input)
      # })
      toReturn <- toReturn +
        geom_bar(stat = 'identity', aes(fill = toHighlight)) +
        scale_fill_manual( values = c( "Yes" = "tomato", "No" = "#5D6D7E" ), guide = "none" ) 
      if (input$bar_chart_type == "Ascending") {
        toReturn <- toReturn +
          labs(
            subtitle = paste("(Ascending order)\n\nDate: ",input$single_date_input,  "(", weekdays(input$single_date_input), ")"),
            title = "Total Entries of Ridership Data",
            x = "Stations",
            y = "Total rides")
      }
      else if (input$bar_chart_type == "Descending") {
        toReturn <- toReturn +
          labs(
            subtitle = paste("(Descending order)\n\nDate: ",input$single_date_input, " (", weekdays(input$single_date_input), ")"),
            title = "Total Entries of Ridership Data",
            x = "Stations",
            y = "Total rides")
      }
      else{
        toReturn <- toReturn +
          labs(
            subtitle = paste("(Alphabetic order)\n\nDate: ",input$single_date_input, "(", weekdays(input$single_date_input), ")"),
            title = "Total Entries of Ridership Data",
            x = "Stations",
            y = "Total rides")
      }
    } else {
      
      pal <- colorNumeric("OrRd", df$rides)
      # print(pal(df$rides))
      
      toReturn <- toReturn +
        geom_bar(stat = 'identity', aes(fill = rides)) +
        scale_fill_gradient2(low = "#fee8c8",
                             high = "#e34a33") +
        theme(legend.position = "none") +
        labs(
          subtitle = paste("\n Date 2: ",input$range_end_date_input, "(", weekdays(input$range_end_date_input), ")\n", "Date 1: ", input$range_start_date_input, "(", weekdays(input$range_start_date_input), ")"),
          title = "Difference of Ridership between two dates (Date 2 - Date 1)",
          x = "Stations",
          y = "Ridership difference (Date 2 - Date 1)")
    }
    
    toReturn
  })
  
  #################################################################
  
  # create a data table
  
  output$mainTable <- renderUI({
    # format the table layout
    
    toReturn <- sum_of_station_df_react_date()
    
    # rename
    names(toReturn)[2] <- "Station"
    names(toReturn)[4] <- "Entries"
    
    colToKeep <- c("Station", "Entries")
    toReturn <- toReturn[colToKeep]
    
    if (input$bar_chart_type == "Ascending") {
      toReturn <- toReturn[order(toReturn$Entries),]
    }
    else if (input$bar_chart_type == "Descending") {
      toReturn <- toReturn[order(rev(toReturn$Entries)),]
    }
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    if (input$range_date_check) {
      names(toReturn)[2] <- "Difference in Entries"
    }
    
    toReturn
    
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            width: inherit !important;
            height: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        toReturn,
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  #################################################################
  
  # create sum of rides for a station
  
  sum_of_station_df_react_date_station <- reactive({
    entries <- NULL
    
    if (input$single_date_check) {
      entries <- array(unlist(
        lapply(unique_all_data_df$stationname,
               sum_of_station_single_date)
      )
      )
    }
    
    if (input$range_date_check) {
      start <- array(unlist(
        lapply(unique_all_data_df$stationname,
               sum_of_station_start_date)
      )
      )
      
      end <- array(unlist(
        lapply(station_names,
               sum_of_station_end_date)
      )
      )
      
      if (input$range_start_date_input > input$range_end_date_input) {
        entries <- array(unlist(Map('-', start, end)))
      } else {
        entries <- array(unlist(Map('-', end, start)))
      }
      
    }
    
    # create a data frame with station names and sum of rides for the stations
    toReturn <- unique_all_data_df
    toReturn$rides <- entries
    
    # add a column to state whether to highlight or not
    toReturn <- toReturn %>% rowwise() %>%
      mutate(toHighlight = if_else(stationname == input$stations, "Yes", "No"))
    
    
    toReturn
  })
  
  sum_of_station_df_react_date <- reactive({
    entries <- NULL
    
    if (input$single_date_check) {
      entries <- array(unlist(
        lapply(unique_all_data_df$stationname,
               sum_of_station_single_date)
      )
      )
    }
    
    if (input$range_date_check) {
      start <- array(unlist(
        lapply(unique_all_data_df$stationname,
               sum_of_station_start_date)
      )
      )
      
      end <- array(unlist(
        lapply(unique_all_data_df$stationname,
               sum_of_station_end_date)
      )
      )
      
      if (input$range_start_date_input > input$range_end_date_input) {
        entries <- array(unlist(Map('-', start, end)))
      } else {
        entries <- array(unlist(Map('-', end, start)))
      }
      
    }
    
    unique_all_data_df$rides <- entries
    
    unique_all_data_df <- unique_all_data_df %>% rowwise() %>%
      mutate(toHighlight = if_else(stationname == input$stations, "Yes", "No"))
    
    unique_all_data_df
  })
  
  sum_of_station_single_date <- function(station) {
    df <- subset(all_data_df, all_data_df$newDate == input$single_date_input)
    sum(df[df$stationname == station,]$rides)
  }
  
  sum_of_station_start_date <- function(station) {
    df <- subset(all_data_df, all_data_df$newDate == input$range_start_date_input)
    sum(df[df$stationname == station,]$rides)
  }
  
  sum_of_station_end_date <- function(station) {
    df <- subset(all_data_df, all_data_df$newDate == input$range_end_date_input)
    sum(df[df$stationname == station,]$rides)
  }
  
  
  
  output$mainMap <- renderLeaflet({
    addCirclesMarkers()
  })
  
  #################################################################
  
  
  
  observeEvent(input$range_date_check, {
    if (input$single_date_check == FALSE & input$range_date_check == FALSE) {
      updateCheckboxInput(session, "single_date_check", value = TRUE)
    }
    
    if (input$range_date_check) {
      updateCheckboxInput(session, "single_date_check", value = FALSE)
      shinyjs::disable("single_date_input")
      shinyjs::enable("range_start_date_input")
      shinyjs::enable("range_end_date_input")
    }
  })
  
  observeEvent(input$prev_day_btn, {
    if (input$single_date_check) {
      updateDateInput(session,
                      "single_date_input",
                      value = as.Date(input$single_date_input) - 1
      )
    }
    
    if (input$range_date_check) {
      updateDateInput(session,
                      "range_start_date_input",
                      value = as.Date(input$range_start_date_input) - 1
      )
      
      updateDateInput(session,
                      "range_end_date_input",
                      value = as.Date(input$range_end_date_input) - 1
      )
    }
  })
  
  observeEvent(input$next_day_btn, {
    if (input$single_date_check) {
      updateDateInput(session,
                      "single_date_input",
                      value = as.Date(input$single_date_input) + 1
      )
    }
    
    if (input$range_date_check) {
      updateDateInput(session,
                      "range_start_date_input",
                      value = as.Date(input$range_start_date_input) + 1
      )
      
      updateDateInput(session,
                      "range_end_date_input",
                      value = as.Date(input$range_end_date_input) + 1
      )
    }
  })
  
  observeEvent(
    {
    input$red
    input$blue
    input$green
    input$pink
    input$orange
    input$yellow
    input$brown
    input$purple
    },
    {
      line_name <- data.frame(stationname=c())
      
      if (input$red == TRUE) {
        line_name <- rbind(line_name,red_s_names)
      }
      
      if (input$blue == TRUE) {
        line_name <- rbind(line_name,blue_s_names)
      }
      
      if (input$green == TRUE) {
        line_name <- rbind(line_name,green_s_names)
      }
      
      if (input$orange == TRUE) {
        line_name <- rbind(line_name,orange_s_names)
      }
      
      if (input$brown == TRUE) {
        line_name <- rbind(line_name,brown_s_names)
      }
      
      if (input$purple == TRUE) {
        line_name <- rbind(line_name,purple_s_names)
      }
      
      if (input$pink == TRUE) {
        line_name <- rbind(line_name,pink_s_names)
      }
      
      if (input$yellow == TRUE) {
        line_name <- rbind(line_name,yellow_s_names)
      }
      
      if(dim(line_name)[2] != 0){
        line_name <- unique(line_name)
        line_name <- line_name[order(line_name$stationname),]

        updateSelectInput(session, "Station",
                          choices = line_name,
        )
      }
  })
  
  observeEvent(input$mainMap_marker_click, {
    click <- input$mainMap_marker_click
    if (is.null(click))
      return()
    
    ### Here get stationname from all_data_df from id, then update with that
    
    updateSelectInput(session, 'stations', selected = click$id)
  })
  
  output$mainMap2 <- renderLeaflet({
    addCirclesMarkers2()
  })
  
  ### Kazi's Part
  
  observeEvent(input$mainMap2_marker_click, {
    click2 <- input$mainMap2_marker_click
    if (is.null(click2))
      return()
    
    ### Here get stationname from id, then update with that
    
    updateSelectInput(session, 'Station', selected = getStationName(click2$id))
  })
  
  
  
  
  temp_halsted <- get_station_df('UIC-Halsted')
  temp_halsted <- do.call(rbind.data.frame, temp_halsted)
  
  halsted_by_year <- temp_halsted %>%
    mutate(year = format(newDate, "%Y")) %>%
    group_by(year) %>%
    summarise(rides = sum(rides))
  halsted_2021 <- temp_halsted %>% filter(year(temp_halsted$newDate) == 2021)
  
  selection <- reactiveValues(station = 'UIC-Halsted', chart = 'Daily', year = 2021,
                              data1 = halsted_by_year, data2 = halsted_2021, color = "#0099f9")
  
  
  observeEvent({
    input$Station
    input$Chart
    input$Year},
    
    {
      selection$year <- input$Year
      selection$chart <- input$Chart
      selection$station <- input$Station
      
      temp <- get_station_df(input$Station)
      temp <- do.call(rbind.data.frame, temp)
      selection$data2 <- temp
      
      temp <- temp %>%
        mutate(year = format(newDate, "%Y"))
      
      shinyjs::hide("RedLine")
      shinyjs::hide("BlueLine")
      shinyjs::hide("GreenLine")
      shinyjs::hide("BrownLine")
      shinyjs::hide("PurpleLine")
      shinyjs::hide("YellowLine")
      shinyjs::hide("PinkLine")
      shinyjs::hide("OrangeLine")
      
      
      
      for (i in 1:nrow(locData)) {
        if (locData[i,6] == getMapID(input$Station)) {
          if (locData[i,8] == "true") {
            updateActionButton(session, "RedLine")
            shinyjs::show("RedLine")
          }
          
          if (locData[i,9] == "true") {
            updateActionButton(session, "BlueLine")
            shinyjs::show("BlueLine")
          }
          
          if (locData[i,10] == "true") {
            updateActionButton(session, "GreenLine")
            shinyjs::show("GreenLine")
          }
          
          if (locData[i,11] == "true") {
            updateActionButton(session, "BrownLine")
            shinyjs::show("BrownLine")
          }
          
          if (locData[i,13] == "true") {
            updateActionButton(session, "PurpleLine")
            shinyjs::show("PurpleLine")
          }
          
          if (locData[i,14] == "true") {
            updateActionButton(session, "YellowLine")
            shinyjs::show("YellowLine")
          }
          
          if (locData[i,15] == "true") {
            updateActionButton(session, "PinkLine")
            shinyjs::show("PinkLine")
          }
          
          if (locData[i,16] == "true") {
            updateActionButton(session, "OrangeLine")
            shinyjs::show("OrangeLine")
          }
        }
      }
      
      selection$data1 <- temp %>%
        mutate(year = format(newDate, "%Y")) %>%
        group_by(year) %>%
        summarise(rides = sum(rides))
      
      selection$color <- "#0099f9"
      
      
      
      print("xx")
      if (input$Chart == "Daily") {
        # print(head(selection$data2))
        # print(input$Year)
        # print(selection$year)
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$newDate) == selection$year)
        print(head(selection$data2))
      }
      
      else if (input$Chart == "Monthly") {
        print("xx")
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$newDate) == selection$year) %>%
          mutate(month = lubridate::month(newDate)) %>%
          group_by(month) %>%
          summarise(rides = sum(rides))
      }
     
      else{
        print("xx")
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$newDate) == selection$year) %>%
          mutate(weekdays = weekdays(newDate)) %>%
          group_by(weekdays) %>%
          summarise(rides = sum(rides))
        
        selection$data2$weekdays <- factor(selection$data2$weekdays, levels = c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        selection$data2 <- selection$data2[order(selection$data2$weekdays), ]
      }
      
      sub <- paste("CTA", selection$chart, "Data:", selection$year, sep = " ")
      
      
      print("xx")
      ### Yearly Bar Chart ###
      output$Station_Yearly_Bar <- renderPlot({
        ggplot(selection$data1, aes(x = year, y = rides)) +
          geom_col(width = 0.7, fill = selection$color) +
          labs(title = selection$station,
               subtitle = "CTA Yearly Data (2001 to 2021)",
               x = "Year",
               y = "Entries") +
          theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
          scale_y_continuous(labels = scales::comma)
      })
      print("xx")
      
      ### Daily Bar Chart ###
      print("inside bar chart1")
      if (selection$chart == 'Daily') {
        print("inside bar chart2")
        # datebreaks <- seq(as.Date(min(selection$data2$newDate)), as.Date(max(selection$data2$newDate)), by = "2 month")
        print("inside bar chart3")
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(newDate, rides)) +
            geom_col(width = 0.7, fill = selection$color) +
            labs(title = selection$station,
                 subtitle = sub,
                 x = "Date", 
                 y = "Entries") +
            scale_x_date(date_labels = "%B" )+
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Monthly Bar Chart ###
      else if (selection$chart == 'Monthly') {
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(x = month.abb[month], y = rides)) +
            geom_col(width = 0.7, fill = selection$color) +
            labs(title = selection$station,
                 subtitle = sub,
                 x = "Month", 
                 y = "Entries") +
            theme(axis.text.x = element_text(vjust = 0.6)) +
            scale_x_discrete(limits = month.abb) + 
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Weekdays Bar Chart ###
      else{
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(x = weekdays, y = rides)) +
            geom_col(width = 0.7, fill = selection$color) +
            labs(title = selection$station,
                 subtitle = sub,
                 x = "Day", 
                 y = "Entries") +
            theme(axis.text.x = element_text(vjust = 0.6)) +
            scale_y_continuous(labels = scales::comma)
        })
      }
      
      ### Yearly Data Table ###
      output$Station_Yearly_Raw <- renderUI({
        
        toReturn <- selection$data1
        
        # rename
        names(toReturn)[1] <- "Year"
        names(toReturn)[2] <- "Entries"
        
        div(
          datatable(toReturn,
                    options = list(
                      pageLength = 10,
                      lengthChange = FALSE,
                      scrollX = TRUE,
                      dom = 'tp',
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    ),
                    rownames = FALSE) %>%
            formatCurrency(2, currency = "", interval = 3, mark = ",") %>%
            formatRound('Entries', digits = 0)
        )
      })
      
      ### Daily Data Table ###
      if (selection$chart == 'Daily') {
        date_data <- selection$data2
        date_data$newDate <- format(date_data$newDate, format = "%m/%d")
        date_data <- date_data[order(date_data$newDate),]
        
        
        output$Station_Raw <- renderUI({
          
          
          toReturn <- date_data[c(5,4)]
          # rename
          names(toReturn)[1] <- "Date"
          names(toReturn)[2] <- "Entries"
          
          div(
            datatable(toReturn,
                      options = list(
                        pageLength = 10,
                        lengthChange = FALSE,
                        scrollX = TRUE,
                        dom = 'tp',
                        columnDefs = list(list(className = 'dt-center', targets = "_all"))
                      ),
                      rownames = FALSE) %>%
              formatCurrency(2, currency = "", interval = 3, mark = ",") %>%
              formatRound('Entries', digits = 0)
          )
        })
      }
      ### Monthly Data Table ###
      else if (selection$chart == 'Monthly') {
        monthly_data = data.frame(month = month.name[selection$data2$month],
                                  rides = selection$data2$rides)
        
        
        output$Station_Raw <- renderUI({
          
          toReturn <- monthly_data
          
          # rename
          names(toReturn)[1] <- "Month"
          names(toReturn)[2] <- "Entries"
          
          div(
            datatable(toReturn,
                      options = list(
                        pageLength = 10,
                        lengthChange = FALSE,
                        scrollX = TRUE,
                        dom = 'tp',
                        columnDefs = list(list(className = 'dt-center', targets = "_all"))
                      ),
                      rownames = FALSE) %>%
              formatCurrency(2, currency = "", interval = 3, mark = ",") %>%
              formatRound('Entries', digits = 0)
          )
        })
      }
      ### Weekdays Data Table ###
      else{
        output$Station_Raw <- renderUI({
          
          toReturn <- selection$data2
          
          # rename
          names(toReturn)[1] <- "Day"
          names(toReturn)[2] <- "Entries"
          
          div(
            datatable(toReturn,
                      options = list(
                        pageLength = 10,
                        lengthChange = FALSE,
                        scrollX = TRUE,
                        dom = 'tp',
                        columnDefs = list(list(className = 'dt-center', targets = "_all"))
                      ),
                      rownames = FALSE) %>%
              formatCurrency(2, currency = "", interval = 3, mark = ",") %>%
              formatRound('Entries', digits = 0)
          )
        })
      }
    })
  
  output$AboutOut <- renderText({
    "Created by: Akash Magnadia & Kazi Shahrukh Omar\n
         Created: XX March, 2022\n
         Data Source:\n
         1. Location data of CTA L Stations: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f\n
         2. Ridership data of CTA L Stations: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f\n
         Data Category: Transportation\n
         Data Owner: Chicago Transit Authority\n
         Intended for visualizing the trends and interesting patterns in Chicago 'L' Station ridership data over the years (2001-2021)."   
  })
  
  
}

shinyApp(ui = ui, server = server)