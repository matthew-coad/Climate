library(tidyverse)
library(reshape2)
library(akima)
# library(gganimate)
library(ggmap)
library(RColorBrewer)
library(animation)


vicMap_load <- function() {
  vic_center <- geocode("Victoria, AU")
  get_map(c(lon=vic_center$lon, lat=vic_center$lat), zoom = 6, maptype = "toner", source = "stamen")
}

# Coordinates with a station by year
## stations_year <- stations %>% full_join(annual, by = "station") %>% group_by(station, long,lat, year) %>% select(station, long, lat, ye

stations_Map <- function(.map, .year) {
  year_stations <- stations %>% full_join(annual, by = "station") %>% filter(year == .year) %>% group_by(station, long,lat, year) %>% select(station, long, lat, year)
  ggmap(.map) + geom_point(data = year_stations, aes(x = long, y = lat)) + ggtitle(paste0("Stations ", .year)) + xlab("Long") + ylab("Lat") + coord_map()
}

stations_Video <- function(.map) {
  year_stations <- stations %>% full_join(annual, by = "station") %>% group_by(station, long,lat, year) %>% select(station, long, lat, year)
  full <- ggmap(.map) + geom_point(data = year_stations, aes(x = long, y = lat, frame=year)) + xlab("Long") + ylab("Lat") + coord_map()
  gganimate(full, interval = 0.1)
}

rain_Plot <- function(yr) {
  map <- vic_map
  max <- 2000
  min <- 0
  breaks = seq(min, max, 500)
  df <- stations %>% full_join(rain_annual, by = "station") %>% filter(!is.na(annual) & year == yr) %>% group_by(long, lat) %>% summarise(mean = mean(annual))
  mat <- interp(x = df$long, y = df$lat, z = df$mean)
  reg <- melt(mat$z, na.rm = TRUE)
  reg$long <- mat$x[reg$Var1]
  reg$lat <- mat$y[reg$Var2]
  reg$year <- yr
  
  plot <- 
    ggmap(map) +
    geom_tile(data = reg, aes(x = long, y = lat, fill = value), alpha = 0.6) +
    stat_contour(data = reg, aes(x = long, y = lat, z = value), breaks = breaks) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(title=paste0("Rain ", yr)) +
    scale_fill_gradientn("mm", colors=brewer.pal(11, "Spectral"), limits = c(min,max), breaks = breaks) +
    coord_map()
  plot
}

rain_video <- function() {
  yrs <- 2000:2005

  rain_animate <- function() {
    walk(yrs, function(yr) print(rain_Plot(yr)))
  }
  
  #save all iterations into one GIF
  saveVideo(rain_animate(), interval = .2, movie.name="Reports/rain.mp4")
}

percent_Plot <- function(yr) {
  map <- vic_map
  max <- 100
  min <- 0
  breaks = seq(min, max, 20)
  df <- stations %>% inner_join(annual, by = "station") %>% group_by(long, lat, year) %>% summarise(annual = mean(annual)) %>% group_by(long, lat) %>% mutate(percent = percent_rank(annual) * 100) %>% filter(year == yr)
  mat <- interp(x = df$long, y = df$lat, z = df$percent)
  reg <- melt(mat$z, na.rm = TRUE)
  reg$long <- mat$x[reg$Var1]
  reg$lat <- mat$y[reg$Var2]
  reg$year <- yr
  
  plot <- 
    ggmap(map) +
    geom_tile(data = reg, aes(x = long, y = lat, fill = value), alpha = 0.6) +
    stat_contour(data = reg, aes(x = long, y = lat, z = value), breaks = breaks) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(title=paste0("Rain Percentile ", yr)) +
    scale_fill_gradientn("%", colors=brewer.pal(11, "Spectral"), limits = c(min,max), breaks = breaks) +
    coord_map()
  plot
}

percent_video <- function() {
  yrs <- 1900:2016
  
  animate <- function() {
    walk(yrs, function(yr) print(percent_Plot(yr)))
  }
  
  #save all iterations into one GIF
  saveVideo(animate(), interval = .2, video.name="rain_percent.mp4")
}

rainMonthly_Plot <- function(yr,mth) {
  map <- vic_map
  max <- 200
  min <- 0
  breaks = seq(min, max, 50)
  df <- stations %>% full_join(monthly, by = "station") %>% filter(!is.na(rain) & year == yr & month == mth) %>% group_by(long, lat) %>% summarise(mean = mean(rain))
  mat <- interp(x = df$long, y = df$lat, z = df$mean)
  reg <- melt(mat$z, na.rm = TRUE)
  reg$long <- mat$x[reg$Var1]
  reg$lat <- mat$y[reg$Var2]
  reg$year <- yr
  reg$month <- mth
  
  plot <- 
    ggmap(map) +
    geom_tile(data = reg, aes(x = long, y = lat, fill = value), alpha = 0.6) +
    stat_contour(data = reg, aes(x = long, y = lat, z = value), breaks = breaks) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(title=paste0("Rain ", yr, " month ", mth)) +
    scale_fill_gradientn("mm", colors=brewer.pal(11, "Spectral"), limits = c(min,max), breaks = breaks) +
    coord_map()
  plot
}

rainMonthly_video <- function(years) {
  months <- levels(monthly$month)
  frames <- cross2(years, months)
  animate <- function() {
    walk(frames, ~ print(rainMonthly_Plot(.[[1]], .[[2]])))
  }
  
  #save all iterations into one GIF
  saveVideo(animate(), interval = .2, video.name="rainmonthly.mp4")
}

monthlyPercent_Plot <- function(yr,mth) {
  map <- vic_map
  max <- 100
  min <- 0
  breaks = seq(min, max, 20)
  df <- 
    stations %>% 
    inner_join(monthly, by = "station") %>% 
    group_by(long, lat, year, month) %>% 
    summarise(rain = mean(rain)) %>% 
    group_by(long, lat) %>% 
    mutate(percent = percent_rank(rain) * 100) %>% 
    filter(year == yr & month == mth)
  mat <- interp(x = df$long, y = df$lat, z = df$percent)
  reg <- melt(mat$z, na.rm = TRUE)
  reg$long <- mat$x[reg$Var1]
  reg$lat <- mat$y[reg$Var2]
  reg$year <- yr
  
  plot <- 
    ggmap(map) +
    geom_tile(data = reg, aes(x = long, y = lat, fill = value), alpha = 0.6) +
    stat_contour(data = reg, aes(x = long, y = lat, z = value), breaks = breaks) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(title=paste0("Rain Percentile ", yr, " ", mth)) +
    scale_fill_gradientn("%", colors=brewer.pal(11, "Spectral"), limits = c(min,max), breaks = breaks) +
    coord_map()
  plot
}

monthlyPercent_video <- function(years) {
  months <- levels(monthly$month)
  frames <- cross2(years, months)
  animate <- function() {
    walk(frames, ~ print(monthlyPercent_Plot(.[[1]], .[[2]])))
  }
  
  #save all iterations into one GIF
  saveVideo(animate(), interval = .1, video.name="rainmonthly.mp4")
}

