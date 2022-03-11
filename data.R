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

unique((all_data_df[is.na(all_data_df$long),])$stationname)

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
default_zoom <- 10


