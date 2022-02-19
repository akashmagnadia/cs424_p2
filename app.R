#libraries to include

library(shiny)
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

# # assume all of the tsv files in this directory are data of the same kind that I want to visualize
# entriesData <- do.call(rbind, lapply(list.files(pattern = "*Totals.tsv"), read.delim))
# 
# locData <- do.call(rbind, lapply(list.files(pattern = "*Stops.tsv"), read.delim))
# locData <- data.frame(locData)
# 
# # convert the dates to the internal format
# entriesData$fullDate <- entriesData$date
# entriesData$newDate <- as.Date(entriesData$fullDate, "%m/%d/%Y")
# entriesData$Date <- NULL
# 
# # add year day month column
# entriesData <- separate(data = entriesData, col = date, into = c("month", "date", "year"), sep = "/")
# 
# # convert the columns to numeric
# entriesData[ c("month", "date", "year")] <- sapply(entriesData[ c("month", "date", "year")],as.numeric)
# 
# add_monthChar <- function(df) {
#   # add new column for month that contains char
#   df$monthChar[df$month == 1] <- "Jan"
#   df$monthChar[df$month == 2] <- "Feb"
#   df$monthChar[df$month == 3] <- "Mar"
#   df$monthChar[df$month == 4] <- "Apr"
#   df$monthChar[df$month == 5] <- "May"
#   df$monthChar[df$month == 6] <- "Jun"
#   df$monthChar[df$month == 7] <- "Jul"
#   df$monthChar[df$month == 8] <- "Aug"
#   df$monthChar[df$month == 9] <- "Sep"
#   df$monthChar[df$month == 10] <- "Oct"
#   df$monthChar[df$month == 11] <- "Nov"
#   df$monthChar[df$month == 12] <- "Dec"
# 
#   df
# }
# 
# entriesData <- add_monthChar(entriesData)
# 
# # parse to days in the week
# entriesData$dayChar <- weekdays(entriesData$newDate)
# 
# # add new column for week that contains int
# add_dayCharToDay <- function(df) {
#   df$day[df$dayChar == "Monday"] <- 1
#   df$day[df$dayChar == "Tuesday"] <- 2
#   df$day[df$dayChar == "Wednesday"] <- 3
#   df$day[df$dayChar == "Thursday"] <- 4
#   df$day[df$dayChar == "Friday"] <- 5
#   df$day[df$dayChar == "Saturday"] <- 6
#   df$day[df$dayChar == "Sunday"] <- 7
# 
#   df
# }
# entriesData <- add_dayCharToDay(entriesData)
# 
# # add new column for char entries that contains comma in the number
# entriesData$ridesChar <- formatC(entriesData$rides, format = "d", big.mark = ",")
# 
# # turn to data frame
# all_data_df <- data.frame(entriesData)
# 
# all_data_df$stationname[all_data_df$stationname == "OHare Airport"] <- "O'Hare"
# 
# # transfer data from location file to main data frame
# all_data_df$long <- locData$long[match(all_data_df$stationname, locData$STATION_NAME)]
# all_data_df$lat <- locData$lat[match(all_data_df$stationname, locData$STATION_NAME)]
# 
# # getting rid of the stations with no location info
# all_data_df <- subset(all_data_df, !is.na(long))
# 
# all_data_df$RedLine <- locData$RED[match(all_data_df$stationname, locData$STATION_NAME)]
# all_data_df$BlueLine <- locData$BLUE[match(all_data_df$stationname, locData$STATION_NAME)]
# all_data_df$GreenLine <- locData$G[match(all_data_df$stationname, locData$STATION_NAME)]
# all_data_df$BrownLine <- locData$BRN[match(all_data_df$stationname, locData$STATION_NAME)]
# all_data_df$PurpleLine <- locData$Pexp[match(all_data_df$stationname, locData$STATION_NAME)]
# all_data_df$YellowLine <- locData$Y[match(all_data_df$stationname, locData$STATION_NAME)]
# all_data_df$PinkLine <- locData$Pnk[match(all_data_df$stationname, locData$STATION_NAME)]
# all_data_df$OrangeLine <- locData$O[match(all_data_df$stationname, locData$STATION_NAME)]
# 
# Red_df <- subset(all_data_df, all_data_df$RedLine == "true")
# Blue_df <- subset(all_data_df, all_data_df$BlueLine == "true")
# Green_df <- subset(all_data_df, all_data_df$GreenLine == "true")
# Brown_df <- subset(all_data_df, all_data_df$BrownLine == "true")
# Purple_df <- subset(all_data_df, all_data_df$PurpleLine == "true")
# Yellow_df <- subset(all_data_df, all_data_df$YellowLine == "true")
# Pink_df <- subset(all_data_df, all_data_df$PinkLine == "true")
# Orange_df <- subset(all_data_df, all_data_df$OrangeLine == "true")
# 
# station_names <- unique(all_data_df[c("stationname")])
# station_names <- station_names[order(station_names$stationname), ]
# 
# # create data frame for a particular station
# create_station_df <- function(station) {
#   subset(all_data_df, all_data_df$stationname == station)
# }
# 
# # get data frame for a particular station
# get_station_df <- function(station) {
#   station_df[[1]][which(station_names == station)]
# }
# 
# # create a data frame for each station
# station_df <- list(lapply(station_names, create_station_df))
# 
# every_nth = function(n) {
#   return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
# }

# position to view all CTA stations on map
default_long <- -87.658753
default_lat <- 41.866568
default_zoom <- 12

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
                     menuItem("About", tabName = "About"),
                     menuItem("Visualization", tabName = "viz", selected = T)
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
                                label = "Range of Date(s)",
                                value = FALSE
                  ),
                  dateInput("range_start_date_input",
                            label = "Start Date",
                            value = "2020-08-23",
                            min = "2001-01-01",
                            max = "2021-11-30",
                            format = "m / dd / yyyy"
                  ),
                  dateInput("range_end_date_input",
                            label = "End Date",
                            value = "2021-08-23",
                            min = "2001-01-01",
                            max = "2021-11-30",
                            format = "m / dd / yyyy"
                  ),
                  selectInput("bar_chart_type",
                              "Sort Bar Chart",
                              choices = c("Alphabetical", "Minimum", "Maximum"),
                              selected = c("Minimum")),
                  selectInput("stations",
                              label = "Stations",
                              choices = station_names,
                              selected = "UIC-Halsted"
                  ),
                  selectInput("map_layer_type",
                              "Map Layer",
                              choices = c("Default", "Light", "Dark"),
                              selected = c("Default")),
                  actionButton("prev_day_btn",
                               label = "Previous Day"
                  ),
                  actionButton("next_day_btn",
                               label = "Next Day"
                  ),
                  width = 1
                ),
                mainPanel(
                  tags$style(type = "text/css", "#mainMap {height: calc(100vh - 80px) !important;}"),
                  tags$style(type = "text/css", "#mainBarGraph {height: calc(70vh - 80px) !important;}"),
                  tags$style(HTML(' .leaflet-top { top: 50%; } ')),
                  fluidRow(
                    column(6,
                           leafletOutput("mainMap")),
                    column(6,
                           div(plotOutput("mainBarGraph")),
                           uiOutput("mainTable"))
                  ),
                  width = 11
                  )
                )
              )
    )
  )
)

server <- function(input, output, session) {
  
  #################################################################
  
  # TODO: Chaange color palatte for the lines
  
  getLowGradientCol <- function(x) {
    gradientCol <- "grey"
    
    if (x == "UIC-Halsted") {
      gradientCol <- "#fbb4ae"
    } else if (x == "O'Hare Airport") {
      gradientCol <- "#b3cde3"
    } else if (x == "Rosemont") {
      gradientCol <- "#ccebc5"
    } else {
      gradientCol <- "#decbe4"
    }
  }
  
  getHighGradientCol <- function(x) {
    gradientCol <- "black"
    
    if (x == "UIC-Halsted") {
      gradientCol <- "#e41a1c"
    } else if (x == "O'Hare Airport") {
      gradientCol <- "#377eb8"
    } else if (x == "Rosemont") {
      gradientCol <- "#4daf4a"
    } else {
      gradientCol <- "#984ea3"
    }
  }  
  
  #################################################################
  
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
    
    m <- m %>% addCircleMarkers(data = locData, ~long, ~lat, 
                                popup = locData$STATION_NAME, 
                                weight = 3, 
                                radius = 10,
                                color = "#70FFF8", 
                                stroke = F, 
                                fillOpacity = 0.50,
                                layerId = locData$STATION_NAME)
    
    m
  }
  
  #################################################################
  
  # create a bar graph
  
  output$mainBarGraph <- renderPlot({
    
    df = sum_of_station_df()
    toReturn <- NULL
    
    if (input$bar_chart_type == "Alphabetical") {
      toReturn <- ggplot(data = df, aes(x = station_names, y = rides)) + 
        geom_bar(stat = 'identity', aes(fill = toHighlight)) +
        scale_x_discrete(limits = rev) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
        labs(x = "Stations",
             y = "Entries") +
        ggtitle(paste("Stations in alphabetical order")) +
        theme(legend.position = "none") + 
        coord_flip() +
        scale_fill_manual( values = c( "Yes" = "tomato", "No" = "gray" ), guide = FALSE )
    }
    
    if (input$bar_chart_type == "Minimum") {
      toReturn <- ggplot(data = df, aes(x = reorder(station_names, rides), y = rides)) + 
        geom_bar(stat = 'identity', aes(fill = toHighlight)) +
        scale_x_discrete(limits = rev) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
        labs(x = "Stations",
             y = "Decrease and Increase in Entries") +
        ggtitle(paste("Stations in order of lowest to highest riders")) +
        theme(legend.position = "none") + 
        coord_flip() +
        scale_fill_manual( values = c( "Yes" = "tomato", "No" = "gray" ), guide = FALSE )
    }
    
    if (input$bar_chart_type == "Maximum") {
      toReturn <- ggplot(data = df, aes(x = reorder(station_names, -rides), y = rides)) + 
        geom_bar(stat = 'identity', aes(fill = toHighlight)) +
        scale_x_discrete(limits = rev) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma) +
        labs(x = "Stations",
             y = "Decrease and Increase in Entries") +
        ggtitle(paste("Stations in order of highest to lowest riders")) +
        theme(legend.position = "none") + 
        coord_flip() +
        scale_fill_manual( values = c( "Yes" = "tomato", "No" = "gray" ), guide = FALSE )
    }
    
    toReturn
  })
  
  #################################################################
  
  # create a data table
  
  output$mainTable <- renderUI({
    # format the table layout
    
    toReturn <- sum_of_station_df()
    
    # rename
    names(toReturn)[1] <- "Station"
    names(toReturn)[2] <- "Entries"
    
    colToKeep <- c("Station", "Entries")
    toReturn <- toReturn[colToKeep]
    
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
  
  sum_of_station_df <- reactive({
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
      
      rides <- array(unlist(Map('-', end, start)))
    }
    
    # create a data frame with station names and sum of rides for the stations
    toReturn <- data.frame(station_names, rides)
    
    # add a column to state whether to highlight or not
    toReturn <- toReturn %>% rowwise() %>%
      mutate(toHighlight = if_else(station_names == input$stations, "Yes", "No"))
    
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
  
  #################################################################
  
  # create a data frame to show yearly data for the station(s) selected
  
  entries_year_sum_table_2 <- reactive({
    toReturn <- sum_of_year_df_2()
    
    # rename
    names(toReturn)[1] <- "Year"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  entries_year_sum_table_3 <- reactive({
    toReturn <- sum_of_year_df_3()
    
    # rename
    names(toReturn)[1] <- "Year"
    names(toReturn)[2] <- "Entries"
    
    # add comma - turns into char
    toReturn$Entries <- formatC(toReturn$Entries, format = "d", big.mark = ",")
    
    toReturn
  })
  
  #################################################################
  
  # create a data table to show yearly data for the 
  
  output$entries_year_sum_table_2 <- renderUI({
    # format the table layout
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        entries_year_sum_table_2(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  output$entries_year_sum_table_3 <- renderUI({
    # format the table layout
    div(
      tags$head(
        tags$style(
          HTML('
          .datatables {
            height: unset !important;
            width: inherit !important;
          }
           ')
        )
      ),
      
      datatable(
        entries_year_sum_table_3(),
        options = list(
          pageLength = 6,
          scrollX = TRUE,
          dom = 'tp',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE
      )
    )
  })
  
  #################################################################
  
  output$mainMap <- renderLeaflet({
    addCirclesMarkers()
  })
  
  #################################################################
  
  observeEvent(input$single_date_check, {
    if (input$single_date_check) {
      updateCheckboxInput(session, "range_date_check", value = FALSE)
      shinyjs::enable("single_date_input")
      shinyjs::disable("range_start_date_input")
      shinyjs::disable("range_end_date_input")
    }
  })
  
  observeEvent(input$range_date_check, {
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
    updateSelectInput(session, 'stations', selected = click$id)
  })
}

shinyApp(ui = ui, server = server)