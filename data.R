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
