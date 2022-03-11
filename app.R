#libraries to include

library(shiny)
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
locData <- data.frame(locData)

entriesData$newDate <- as.Date(entriesData$date, "%m/%d/%Y")
entriesData$date <- NULL
entriesData$stationname[entriesData$stationname == "Skokie"] <- "Dempster-Skokie"

all_data_df <- entriesData

# transfer data from location file to main data frame
all_data_df$long <- locData$long[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$lat <- locData$lat[match(all_data_df$station_id, locData$MAP_ID)]

# getting rid of the stations with no location info
all_data_df <- subset(all_data_df, !is.na(long))

all_data_df$RedLine <- locData$RED[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$BlueLine <- locData$BLUE[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$GreenLine <- locData$G[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$BrownLine <- locData$BRN[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$PurpleLine <- locData$Pexp[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$YellowLine <- locData$Y[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$PinkLine <- locData$Pnk[match(all_data_df$station_id, locData$MAP_ID)]
all_data_df$OrangeLine <- locData$O[match(all_data_df$station_id, locData$MAP_ID)]

Red_df <- subset(all_data_df, all_data_df$RedLine == "true")
Blue_df <- subset(all_data_df, all_data_df$BlueLine == "true")
Green_df <- subset(all_data_df, all_data_df$GreenLine == "true")
Brown_df <- subset(all_data_df, all_data_df$BrownLine == "true")
Purple_df <- subset(all_data_df, all_data_df$PurpleLine == "true")
Yellow_df <- subset(all_data_df, all_data_df$YellowLine == "true")
Pink_df <- subset(all_data_df, all_data_df$PinkLine == "true")
Orange_df <- subset(all_data_df, all_data_df$OrangeLine == "true")

station_names <- unique(all_data_df[c("stationname")])
station_names <- station_names[order(station_names$stationname), ]

# merge location data information of different lines
for (i in 1:nrow(locData)) {
  for (j in 1:nrow(locData)) {
    if (locData[i,4] == locData[j,4]) {
      
      # Red Line
      if (locData[i,8] == "true") {
        locData[j,8] = "true"
      }
      
      # Blue Line
      if (locData[i,9] == "true") {
        locData[j,9] = "true"
      }
      
      # Green Line
      if (locData[i,10] == "true") {
        locData[j,10] = "true"
      }
      
      # Brown Line
      if (locData[i,11] == "true") {
        locData[j,11] = "true"
      }
      
      # Purple Line
      if (locData[i,13] == "true") {
        locData[j,13] = "true"
      }
      
      # Yellow Line
      if (locData[i,14] == "true") {
        locData[j,14] = "true"
      }
      
      # Pink Line
      if (locData[i,15] == "true") {
        locData[j,15] = "true"
      }
      
      # Orange Line
      if (locData[i,16] == "true") {
        locData[j,16] = "true"
      }
    }
  }
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
      tabItem(tabName = "viz",
              
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
              tags$style(type = "text/css", "#mainMap2 {height: calc(80vh) !important;}"),
              # tags$style(HTML(' .leaflet-top { top: 50%; } ')),
              
              fluidRow(
                
                column(4,
                       br(),br(),
                       
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
                                    plotOutput("Station_Yearly_Bar")
                                )
                         ),
                         column(6, 
                                box(solidHeader = TRUE, status = "primary", width = 200,
                                    plotOutput("Station_Bar")
                                )
                         ),
                         
                         fluidRow(
                           style = "padding: 15px;",
                           column(4,
                                  p("Choose a station"),
                                  selectInput("Station", NULL,
                                              choices = station_names, 
                                              selected = "UIC-Halsted"
                                  )
                           ),
                           
                           column(2,offset = 2,
                                  p("Choose chart type"),
                                  selectInput("Chart", NULL,
                                              choices = c("Daily", "Monthly", "Weekdays"), 
                                              selected = "Daily"
                                  )
                           ),
                           column(2,
                                  p("Choose year"),
                                  selectInput("Year", NULL,
                                              choices = c(2001:2021), 
                                              selected = 2021
                                  )
                           ),
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
    
    # locData$Entries[df$station_names == locData$STATION_NAME] <- df$rides
    dataToUse <- merge(locData, df, by.x = "STATION_NAME", 
                       by.y = "station_names", all.x = TRUE, all.y = FALSE)
    
    dataToUse['popUp'] <- "No ridernship data"
    print("Merged")
    print(dataToUse)
    
    for (i in 1:nrow(dataToUse))
      {
      # 21 is the index where popUp is
        dataToUse[i,21] <- paste0("Station Name: ", dataToUse[i,1], "<br>", "Entries: ", dataToUse[i,20], "<br>", "Line: ")
        
        if (dataToUse[i,8] == "true") {
          # Red Line
          dataToUse[i,21] <- paste0(dataToUse[i,21], "Red | ")
        }
        
        if (dataToUse[i,9] == "true") {
          # Blue Line
          dataToUse[i,21] <- paste0(dataToUse[i,21], "Blue | ")
        }
        
        if (dataToUse[i,10] == "true") {
          # Green Line
          dataToUse[i,21] <- paste0(dataToUse[i,21], "Green | ")
        }
        
        if (dataToUse[i,11] == "true") {
          # Brown Line
          dataToUse[i,21] <- paste0(dataToUse[i,21], "Brown | ")
        }
        
        if (dataToUse[i,13] == "true") {
          # Purple Line
          dataToUse[i,21] <- paste0(dataToUse[i,21], "Purple | ")
        }
        
        if (dataToUse[i,14] == "true") {
          # Yellow Line
          dataToUse[i,21] <- paste0(dataToUse[i,21], "Yellow | ")
        }
        
        if (dataToUse[i,15] == "true") {
          # Pink Line
          dataToUse[i,21] <- paste0(dataToUse[i,21], "Pink | ")
        }
        
        if (dataToUse[i,16] == "true") {
          # Orange Line
          dataToUse[i,21] <- paste0(dataToUse[i,21], "Orange | ")
        }
        
        print(dataToUse[i,21])
      }
    
    print("Before M")
    print(dataToUse)
    
    m <- m %>% addCircleMarkers(data = dataToUse, ~long, ~lat,
                                popup = dataToUse$popUp,
                                weight = 3,
                                radius = 10,
                                color = "black",
                                fillColor = "#70FFF8",
                                stroke = T,
                                fillOpacity = 0.50,
                                layerId = dataToUse$STATION_NAME)
    
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
    
    #print(head(df))
    
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
        scale_fill_manual( values = c( "Yes" = "tomato", "No" = "#5D6D7E" ), guide = FALSE ) 
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
      toReturn <- toReturn +
        geom_bar(stat = 'identity', aes(fill = rides)) +
        scale_fill_gradient2(low = "#fc8d59",
                             high = "#91cf60",
                             midpoint = median(0)) +
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
    # print(toReturn)
    
    # rename
    names(toReturn)[1] <- "Station"
    names(toReturn)[2] <- "Entries"
    
    colToKeep <- c("Station", "Entries")
    toReturn <- toReturn[colToKeep]
    
    #print(head(toReturn))
    
    if (input$bar_chart_type == "Ascending") {
      toReturn <- toReturn[order(toReturn$Entries),]
    }
    else if (input$bar_chart_type == "Descending") {
      toReturn <- toReturn[order(-toReturn$Entries),]
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
    rides <- NULL
    
    if (input$single_date_check) {
      rides <- array(unlist(
        lapply(station_names,
               sum_of_station_single_date)
      )
      )
    }
    
    if (input$range_date_check) {
      start <- array(unlist(
        lapply(station_names,
               sum_of_station_start_date)
      )
      )
      
      end <- array(unlist(
        lapply(station_names,
               sum_of_station_end_date)
      )
      )
      
      if (input$range_start_date_input > input$range_end_date_input) {
        rides <- array(unlist(Map('-', start, end)))
      } else {
        rides <- array(unlist(Map('-', end, start)))
      }
      
    }
    
    # create a data frame with station names and sum of rides for the stations
    toReturn <- data.frame(station_names, rides)
    
    # add a column to state whether to highlight or not
    toReturn <- toReturn %>% rowwise() %>%
      mutate(toHighlight = if_else(station_names == input$stations, "Yes", "No"))
    
    
    toReturn
  })
  
  sum_of_station_df_react_date <- reactive({
    rides <- NULL
    
    if (input$single_date_check) {
      rides <- array(unlist(
        lapply(station_names,
               sum_of_station_single_date)
      )
      )
    }
    
    if (input$range_date_check) {
      start <- array(unlist(
        lapply(station_names,
               sum_of_station_start_date)
      )
      )
      
      end <- array(unlist(
        lapply(station_names,
               sum_of_station_end_date)
      )
      )
      
      if (input$range_start_date_input > input$range_end_date_input) {
        rides <- array(unlist(Map('-', start, end)))
      } else {
        rides <- array(unlist(Map('-', end, start)))
      }
      
    }
    
    # create a data frame with station names and sum of rides for the stations
    toReturn <- data.frame(station_names, rides)
    
    toReturn
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
  
  observeEvent(input$single_date_check, {
    if (input$single_date_check == FALSE & input$range_date_check == FALSE) {
      updateCheckboxInput(session, "range_date_check", value = TRUE)
    }
    
    if (input$single_date_check) {
      updateCheckboxInput(session, "range_date_check", value = FALSE)
      shinyjs::enable("single_date_input")
      shinyjs::disable("range_start_date_input")
      shinyjs::disable("range_end_date_input")
    }
  })
  
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
  
  observeEvent(input$mainMap_marker_click, {
    click <- input$mainMap_marker_click
    if (is.null(click))
      return()
    # print("here")
    
    ### Here get stationname from all_data_df from id, then update with that
    
    updateSelectInput(session, 'stations', selected = click$id)
  })
  
  output$mainMap2 <- renderLeaflet({
    addCirclesMarkers()
  })
  
  ### Kazi's Part
  
  observeEvent(input$mainMap2_marker_click, {
    click2 <- input$mainMap2_marker_click
    if (is.null(click2))
      return()
    #print("here")
    #print(click2$id)
    #z <- get_station_df(click2$id)
    #print(head(z))
    
    ### Here get stationname from id, then update with that
    
    updateSelectInput(session, 'Station', selected = click2$id)
  })
  
  temp_halsted <- get_station_df('UIC-Halsted')
  temp_halsted <- do.call(rbind.data.frame, temp_halsted)
  # print(head(temp_halsted))
  # print(class(temp_halsted))
  # print(temp_halsted)
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
      
      #print(head(selection$data2))
      
      shinyjs::hide("RedLine")
      shinyjs::hide("BlueLine")
      shinyjs::hide("GreenLine")
      shinyjs::hide("BrownLine")
      shinyjs::hide("PurpleLine")
      shinyjs::hide("YellowLine")
      shinyjs::hide("PinkLine")
      shinyjs::hide("OrangeLine")
      
      for (i in 1:nrow(locData)) {
        if (locData[i,4] == input$Station) {
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
      # 
      # if (temp[1,]$RedLine == "true") {
      #   #b42c2c
      #   updateActionButton(session, "RedLine")
      #   shinyjs::show("RedLine")
      #   
      # }
      # if (temp[1,]$OrangeLine == "true") {
      #   #dd4b26
      #   updateActionButton(session, "OrangeLine")
      #   shinyjs::show("OrangeLine")
      # }
      # if (temp[1,]$GreenLine == "true") {
      #   #359140
      #   updateActionButton(session, "GreenLine")
      #   shinyjs::show("GreenLine")
      # }
      # if (temp[1,]$YellowLine == "true") {
      #   #f0e21b
      #   updateActionButton(session, "YellowLine")
      #   shinyjs::show("YellowLine")
      # }
      # if (temp[1,]$BlueLine == "true") {
      #   #3c95d6
      #   updateActionButton(session, "BlueLine")
      #   shinyjs::show("BlueLine")
      # }
      # if (temp[1,]$BrownLine == "true") {
      #   #4d2c15
      #   updateActionButton(session, "BrownLine")
      #   shinyjs::show("BrownLine")
      # }
      # if (temp[1,]$PurpleLine == "true") {
      #   #482887
      #   updateActionButton(session, "PurpleLine")
      #   shinyjs::show("PurpleLine")
      # }
      # if (temp[1,]$PinkLine == "true") {
      #   #d57a9e
      #   updateActionButton(session, "PinkLine")
      #   shinyjs::show("PinkLine")
      # }
      # actionButton("btn1", label = "Blue Line", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      
      selection$data1 <- temp %>%
        mutate(year = format(newDate, "%Y")) %>%
        group_by(year) %>%
        summarise(rides = sum(rides))
      
      selection$color <- "#0099f9"
      
      updateSelectInput(session, "Year", NULL,
                        choices = c(min(selection$data1$year):max(selection$data1$year)), 
                        selected = max(selection$data1$year)
      )
      
      if (input$Chart == "Daily") {
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$newDate) == input$Year)
      }
      else if (input$Chart == "Monthly") {
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$newDate) == input$Year) %>%
          mutate(month = lubridate::month(newDate)) %>%
          group_by(month) %>%
          summarise(rides = sum(rides))
      }
      else{
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$newDate) == input$Year) %>%
          mutate(weekdays = weekdays(newDate)) %>%
          group_by(weekdays) %>%
          summarise(rides = sum(rides))
        
        selection$data2$weekdays <- factor(selection$data2$weekdays, levels = c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        selection$data2 <- selection$data2[order(selection$data2$weekdays), ]
      }
      
      sub <- paste("CTA", selection$chart, "Data:", selection$year, sep = " ")
      
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
      
      ### Daily Bar Chart ###
      if (selection$chart == 'Daily') {
        
        datebreaks <- seq(as.Date(min(selection$data2$newDate)), as.Date(max(selection$data2$newDate)), by = "2 month")
        #print(max(selection$data2$date))
        
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(newDate, rides)) +
            geom_col(width = 0.7, fill = selection$color) +
            labs(title = selection$station,
                 subtitle = sub,
                 x = "Date", 
                 y = "Entries") +
            scale_x_date(date_labels = "%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(selection$data2$newDate)), as.Date(max(selection$data2$newDate))) ) +
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
        
        
        output$Station_Raw <- renderUI({
          
          
          toReturn <- date_data[c(5,4)]
          #print(head(toReturn))
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