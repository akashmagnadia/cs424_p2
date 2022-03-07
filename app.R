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

# convert the dates to the internal format
entriesData$fullDate <- entriesData$date
entriesData$newDate <- as.Date(entriesData$fullDate, "%m/%d/%Y")
entriesData$Date <- NULL

# add year day month column
entriesData <- separate(data = entriesData, col = date, into = c("month", "date", "year"), sep = "/")

# convert the columns to numeric
entriesData[ c("month", "date", "year")] <- sapply(entriesData[ c("month", "date", "year")],as.numeric)

add_monthChar <- function(df) {
  # add new column for month that contains char
  df$monthChar[df$month == 1] <- "Jan"
  df$monthChar[df$month == 2] <- "Feb"
  df$monthChar[df$month == 3] <- "Mar"
  df$monthChar[df$month == 4] <- "Apr"
  df$monthChar[df$month == 5] <- "May"
  df$monthChar[df$month == 6] <- "Jun"
  df$monthChar[df$month == 7] <- "Jul"
  df$monthChar[df$month == 8] <- "Aug"
  df$monthChar[df$month == 9] <- "Sep"
  df$monthChar[df$month == 10] <- "Oct"
  df$monthChar[df$month == 11] <- "Nov"
  df$monthChar[df$month == 12] <- "Dec"
  
  df
}

entriesData <- add_monthChar(entriesData)

# parse to days in the week
entriesData$dayChar <- weekdays(entriesData$newDate)

# add new column for week that contains int
add_dayCharToDay <- function(df) {
  df$day[df$dayChar == "Monday"] <- 1
  df$day[df$dayChar == "Tuesday"] <- 2
  df$day[df$dayChar == "Wednesday"] <- 3
  df$day[df$dayChar == "Thursday"] <- 4
  df$day[df$dayChar == "Friday"] <- 5
  df$day[df$dayChar == "Saturday"] <- 6
  df$day[df$dayChar == "Sunday"] <- 7
  
  df
}
entriesData <- add_dayCharToDay(entriesData)

# add new column for char entries that contains comma in the number
entriesData$ridesChar <- formatC(entriesData$rides, format = "d", big.mark = ",")

# turn to data frame
all_data_df <- data.frame(entriesData)

all_data_df$stationname[all_data_df$stationname == "OHare Airport"] <- "O'Hare"

station_names <- unique(all_data_df[c("stationname")])
station_names <- station_names[order(station_names$stationname), ]

# only keep stations that I have information about ridership data
locData <- subset(locData, locData$STATION_NAME %in% station_names)

# transfer data from location file to main data frame
all_data_df$long <- locData$long[match(all_data_df$stationname, locData$STATION_NAME)]
all_data_df$lat <- locData$lat[match(all_data_df$stationname, locData$STATION_NAME)]

# getting rid of the stations with no location info
all_data_df <- subset(all_data_df, !is.na(long))

all_data_df$RedLine <- locData$RED[match(all_data_df$stationname, locData$STATION_NAME)]
all_data_df$BlueLine <- locData$BLUE[match(all_data_df$stationname, locData$STATION_NAME)]
all_data_df$GreenLine <- locData$G[match(all_data_df$stationname, locData$STATION_NAME)]
all_data_df$BrownLine <- locData$BRN[match(all_data_df$stationname, locData$STATION_NAME)]
all_data_df$PurpleLine <- locData$Pexp[match(all_data_df$stationname, locData$STATION_NAME)]
all_data_df$YellowLine <- locData$Y[match(all_data_df$stationname, locData$STATION_NAME)]
all_data_df$PinkLine <- locData$Pnk[match(all_data_df$stationname, locData$STATION_NAME)]
all_data_df$OrangeLine <- locData$O[match(all_data_df$stationname, locData$STATION_NAME)]

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
              h1("Ridership Data of all CTA Stations",align="center"),
              br(),br(),
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
                  tags$style(type = "text/css", "#mainMap {height: calc(100vh - 80px) !important;}"),
                  tags$style(type = "text/css", "#mainBarGraph {height: calc(70vh - 80px) !important;}"),
                  tags$style(HTML(' .leaflet-top { top: 50%; } ')),
                  tags$style(HTML('.btn-primary, .btn-primary:hover, .btn-primary:active, .btn-primary:visited {background-color: #8064A2 !important;}')),
                  
                  p("Click on a station on the map to highlight it on the bar chart"),
                  fluidRow(
                    column(5,
                           
                           box(id = "map_container",solidHeader = TRUE, status = "primary", width = 200,
                               leafletOutput("mainMap")
                           )
                    ),
                    
                    column(7,
                           box(solidHeader = TRUE, status = "primary", width = 200,
                               div(plotOutput("mainBarGraph")),
                               uiOutput("mainTable")
                           )
                           
                    )
                  ),
                  width = 10
                )
              )
      ),
      tabItem(tabName = "ovw",
              tags$style(type = "text/css", "#mainMap2 {height: calc(100vh - 80px) !important;}"),
              tags$style(HTML(' .leaflet-top { top: 50%; } ')),
              
              h1("Ridership Data of a Particular CTA Station",align="center"),
              br(),br(),
              fluidRow(
                column(4,
                       p("Choose a station by clicking on it on the map", align='center'),
                       box(solidHeader = TRUE, status = "primary", width=250,
                           leafletOutput("mainMap2")
                       )
                ),
                column(8,
                       fluidRow(
                         column(4,
                                p("Choose a station"),
                                selectInput("Station", NULL,
                                            choices = station_names, 
                                            selected = "UIC-Halsted"
                                )
                         ),
                         
                         column(2,offset=2,
                                p("Choose chart type"),
                                selectInput("Chart", NULL,
                                            choices = c("Daily", "Monthly", "Weekdays"), 
                                            selected = "Daily"
                                )
                         ),
                         column(2,
                                p("Choose year"),
                                selectInput("Year", NULL,
                                            choices=c(2001:2021), 
                                            selected = 2021
                                )
                         ),
                       ),
                       
                       fluidRow(
                         column(1,
                                actionButton("btn1", label=NULL)
                         ),
                         column(1,
                                actionButton("btn2", label=NULL)
                         ),
                         column(1,
                                actionButton("btn3", label=NULL)
                         ),
                         column(1,
                                actionButton("btn4", label=NULL)
                         ),
                         column(1,
                                actionButton("btn5", label=NULL)
                         ),
                         column(1,
                                actionButton("btn6", label=NULL)
                         ),
                         column(1,
                                actionButton("btn7", label=NULL)
                         ),
                         column(1,
                                actionButton("btn8", label=NULL)
                         )
                       ),
                       
                       fluidRow(
                         br(),
                         column(6,
                                box(solidHeader = TRUE, status = "primary", width=200,
                                    plotOutput("Station_Yearly_Bar")
                                )
                         ),
                         column(6, 
                                box(solidHeader = TRUE, status = "primary", width=200,
                                    plotOutput("Station_Bar")
                                )
                         ),
                         column(6,
                                box(solidHeader = TRUE, status = "primary", width = 200,
                                    dataTableOutput("Station_Yearly_Raw")
                                )
                                
                         ),
                         
                         column(6,
                                box(solidHeader = TRUE, status = "primary", width = 200,
                                    dataTableOutput("Station_Raw")
                                )
                         )
                       )
                )
              )
      ),
      
      #About page
      tabItem(tabName="About",
              h2("About Page"),
              verbatimTextOutput("AboutOut")
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
                                color = "black",
                                fillColor = "#70FFF8",
                                stroke = T,
                                fillOpacity = 0.50,
                                layerId = locData$STATION_NAME)
    
    #print(head(all_data_df))
    
    m
  }
  
  #################################################################
  
  # create a bar graph
  
  output$mainBarGraph <- renderPlot({
    
    df = sum_of_station_df()
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
      if (input$bar_chart_type == "Ascending"){
        toReturn <- toReturn +
          labs(
            subtitle=paste("(Ascending order)\n\nDate: ",input$single_date_input,  "(", weekdays(input$single_date_input), ")"),
            title="Total Entries of Ridership Data",
            x = "Stations",
            y = "Total rides")
      }
      else if (input$bar_chart_type == "Descending"){
        toReturn <- toReturn +
          labs(
            subtitle=paste("(Descending order)\n\nDate: ",input$single_date_input, " (", weekdays(input$single_date_input), ")"),
            title="Total Entries of Ridership Data",
            x = "Stations",
            y = "Total rides")
      }
      else{
        toReturn <- toReturn +
          labs(
            subtitle=paste("(Alphabetic order)\n\nDate: ",input$single_date_input, "(", weekdays(input$single_date_input), ")"),
            title="Total Entries of Ridership Data",
            x = "Stations",
            y = "Total rides")
      }
    } else {
      toReturn <- toReturn +
        geom_bar(stat = 'identity', aes(fill = rides)) +
        scale_fill_gradient2(low = "#fc8d59",
                             high = "#91cf60",
                             midpoint = median(0)) +
        theme(legend.position = "none")+
        labs(
          subtitle=paste("\n Date 2: ",input$range_end_date_input, "(", weekdays(input$range_end_date_input), ")\n", "Date 1: ", input$range_start_date_input, "(", weekdays(input$range_start_date_input), ")"),
          title="Difference of Ridership between two dates (Date 2 - Date 1)",
          x = "Stations",
          y = "Ridership difference (Date 2 - Date 1)")
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
    
    #print(head(toReturn))
    
    if (input$bar_chart_type == "Ascending") {
      toReturn <- toReturn[order(toReturn$Entries),]
    }
    else if (input$bar_chart_type == "Descending"){
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
    print("here")
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
    print("here")
    print(click2$id)
    z <- get_station_df(click2$id)
    #print(head(z))
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
  
  selection <- reactiveValues(station='UIC-Halsted', chart='Daily', year=2021,
                              data1=halsted_by_year, data2=halsted_2021, color="#0099f9")
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
      
      shinyjs::hide("btn1") 
      shinyjs::hide("btn2") 
      shinyjs::hide("btn3") 
      shinyjs::hide("btn4") 
      shinyjs::hide("btn5") 
      shinyjs::hide("btn6") 
      shinyjs::hide("btn7") 
      shinyjs::hide("btn8")
      
      i <- 0
      #print(temp[1,]$BlueLine)
      if(temp[1,]$RedLine=="true"){
        i <- i+1
        inputId <- paste("btn",i,sep="")
        #b42c2c
        updateActionButton(session, inputId, label = "Red Line")
        shinyjs::show(inputId)
        #shinyjs::disable(inputId)
        
      }
      if(temp[1,]$OrangeLine=="true"){
        i <- i+1
        inputId <- paste("btn",i,sep="")
        #dd4b26
        updateActionButton(session, inputId, label = "Orange Line")
        shinyjs::show(inputId)
        #shinyjs::disable(inputId)
      }
      if(temp[1,]$GreenLine=="true"){
        i <- i+1
        inputId <- paste("btn",i,sep="")
        #359140
        updateActionButton(session, inputId, label = "Green Line")
        shinyjs::show(inputId)
        #shinyjs::disable(inputId)
      }
      if(temp[1,]$YellowLine=="true"){
        i <- i+1
        inputId <- paste("btn",i,sep="")
        #f0e21b
        updateActionButton(session, inputId, label = "Yellow Line")
        shinyjs::show(inputId)
        #shinyjs::disable(inputId)
      }
      if(temp[1,]$BlueLine=="true"){
        i <- i+1
        inputId <- paste("btn",i,sep="")
        #3c95d6
        updateActionButton(session, inputId, label = "Blue Line")
        shinyjs::show(inputId)
        #shinyjs::disable(inputId)
      }
      if(temp[1,]$BrownLine=="true"){
        i <- i+1
        inputId <- paste("btn",i,sep="")
        #4d2c15
        updateActionButton(session, inputId, label = "Brown Line")
        shinyjs::show(inputId)
        #shinyjs::disable(inputId)
      }
      if(temp[1,]$PurpleLine=="true"){
        i <- i+1
        inputId <- paste("btn",i,sep="")
        #482887
        updateActionButton(session, inputId, label = "Purple Line")
        shinyjs::show(inputId)
        #shinyjs::disable(inputId)
      }
      if(temp[1,]$PinkLine=="true"){
        i <- i+1
        inputId <- paste("btn",i,sep="")
        #d57a9e
        updateActionButton(session, inputId, label = "Pink Line")
        shinyjs::show(inputId)
        #shinyjs::disable(inputId)
      }
      # actionButton("btn1", label = "Blue Line", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      
      selection$data1 <- temp %>%
        mutate(year = format(newDate, "%Y")) %>%
        group_by(year) %>%
        summarise(rides = sum(rides))
      
      selection$color <- "#0099f9"
      
      updateSelectInput(session, "Year", NULL,
                        choices=c(min(selection$data1$year):max(selection$data1$year)), 
                        selected = max(selection$data1$year)
      )
      
      if(input$Chart == "Daily"){
        selection$data2 <- selection$data2 %>% filter(year(selection$data2$newDate) == input$Year)
      }
      else if(input$Chart == "Monthly"){
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
        
        selection$data2$weekdays <- factor(selection$data2$weekdays, levels= c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        selection$data2 <- selection$data2[order(selection$data2$weekdays), ]
      }
      
      sub <- paste("CTA", selection$chart, "Data:", selection$year, sep=" ")
      
      ### Yearly Bar Chart ###
      output$Station_Yearly_Bar <- renderPlot({
        ggplot(selection$data1, aes(x = year, y = rides))+
          geom_col(width = 0.7, fill=selection$color) +
          labs(title=selection$station,
               subtitle="CTA Yearly Data (2001 to 2021)")+
          theme(axis.text.x = element_text(angle=65, vjust=0.6))+
          scale_y_continuous(labels = scales::comma)
      })
      
      ### Daily Bar Chart ###
      if(selection$chart=='Daily'){
        
        datebreaks <- seq(as.Date(min(selection$data2$newDate)), as.Date(max(selection$data2$newDate)), by="2 month")
        #print(max(selection$data2$date))
        
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(newDate, rides)) +
            geom_col(width = 0.7, fill=selection$color) +
            labs(title=selection$station,
                 subtitle=sub,
                 x = "date", y = "rides")+
            scale_x_date(date_labels="%B",
                         breaks = datebreaks,
                         limits = c( as.Date(min(selection$data2$newDate)), as.Date(max(selection$data2$newDate))) )+
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Monthly Bar Chart ###
      else if(selection$chart=='Monthly'){
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(x = month.abb[month], y = rides))+
            geom_col(width = 0.7, fill=selection$color) +
            labs(title=selection$station,
                 subtitle=sub,
                 x='month')+
            theme(axis.text.x = element_text(vjust=0.6))+
            scale_x_discrete(limits = month.abb)+
            scale_y_continuous(labels = scales::comma)
        })
      }
      ### Weekdays Bar Chart ###
      else{
        output$Station_Bar <- renderPlot({
          ggplot(selection$data2, aes(x = weekdays, y = rides))+
            geom_col(width = 0.7, fill=selection$color) +
            labs(title=selection$station,
                 subtitle=sub)+
            theme(axis.text.x = element_text(vjust=0.6))+
            scale_y_continuous(labels = scales::comma)
        })
      }
      
      ### Yearly Data Table ###
      output$Station_Yearly_Raw <- renderDataTable(
        datatable(selection$data1,
                  options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15)
                  )) %>%
          formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
          formatRound('rides', digits = 0)
      )
      
      ### Daily Data Table ###
      if(selection$chart=='Daily'){
        date_data <- selection$data2
        date_data$newDate <-format(date_data$newDate, format="%m/%d")
        output$Station_Raw <- renderDataTable(
          datatable(date_data[c(9,7)],
                    options = list(searching = FALSE,pageLength = 10, lengthMenu = c(5, 10, 15, 20, 25, 30)
                    )) %>%
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      ### Monthly Data Table ###
      else if(selection$chart=='Monthly'){
        monthly_data = data.frame(month=month.name[selection$data2$month],
                                  rides=selection$data2$rides)
        output$Station_Raw <- renderDataTable(
          datatable(monthly_data,
                    options = list(searching = FALSE,pageLength = 12, lengthChange=FALSE
                    )) %>%
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
      }
      ### Weekdays Data Table ###
      else{
        output$Station_Raw <- renderDataTable(
          datatable(selection$data2,
                    options = list(searching = FALSE,pageLength = 7, lengthChange=FALSE
                    )) %>%
            formatCurrency(2, currency = "", interval = 3, mark = ",")%>%
            formatRound('rides', digits = 0)
        )
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