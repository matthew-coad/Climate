library(maps)
library(purrr)
library(lubridate)

maps_datapath <- function() climate_datapath("Maps")

stations <- get_stations_AUS()
max_temp <- get_max_temp_AUS()

# GISTEMP Base line 1951-1980  
baseline_range <- c(1951, 1980)

get_baseline_stations <- function(stations, max_temp, baseline_start, baseline_end) {
    
    baseline_stations_years <- 
        max_temp %>% 
        group_by(Zone, Station) %>% 
        summarise(MinYear = min(year(Date)), MaxYear = max(year(Date))) %>% 
        filter(MinYear <= baseline_start, MaxYear >= baseline_end)
    
    baseline_stations <- stations %>% inner_join(baseline_stations_years, by = c("Zone", "Station"))
    baseline_stations
}

baseline_stations <- get_baseline_stations(stations, max_temp, baseline_range[1], baseline_range[2])

map_aus <- borders("world", regions = "Australia",  fill = "grey90", colour = "black")

ggplot(baseline_stations) +
    map_aus +
    geom_point(aes(x = Longitude, y = Latitude), na.rm=TRUE) +
    xlim(112, 155) +  # Set x axis limits, xlim(min, max)
    ylim(-44, -10) +  # Set y axis limits
    coord_quickmap() +  # Define aspect ratio of the map, so it doesn't get stretched when resizing
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude")


get_max_temp_baseline <- function(baseline_stations, max_temp, baseline_start, baseline_end) {

    baseline_max_temp_days <- 
        max_temp %>% 
        inner_join(baseline_stations, by = c("Zone", "Station")) %>% 
        select(-Max_temp.Quality, -Station.Name, -MinYear, -MaxYear) %>%
        mutate(year = year(Date), doy = yday(Date), jday = as.integer(Date)) %>%
        arrange(Zone, Station, jday) %>%
        mutate(
            Lag1 = lag(Max_temp, 1), Lag2 = lag(Max_temp, 2), Lag3 = lag(Max_temp, 3), Lag4 = lag(Max_temp, 4), Lag5 = lag(Max_temp, 5),
            Lead1 = lead(Max_temp, 1), Lead2 = lead(Max_temp, 2), Lead3 = lead(Max_temp, 3), Lead4 = lead(Max_temp, 4), Lead5 = lead(Max_temp, 5)
        )
    
    baseline_max_temp <- baseline_max_temp_days %>% 
        filter(year >= start_year, year <= end_year) %>% 
        group_by(Zone, Station, doy) %>% 
        summarise(
            base_max_temp0 = mean(c(Max_temp)), 
            base_max_temp1 = mean(c(Max_temp, Lag1, Lead1), na.rm = TRUE),
            base_max_temp2 = mean(c(Max_temp, Lag1, Lead1, Lag2, Lead2), na.rm = TRUE),
            base_max_temp3 = mean(c(Max_temp, Lag1, Lead1, Lag2, Lead2, Lag3, Lead3), na.rm = TRUE),
            base_max_temp4 = mean(c(Max_temp, Lag1, Lead1, Lag2, Lead2, Lag3, Lead3, Lag4, Lead4), na.rm = TRUE),
            base_max_temp5 = mean(c(Max_temp, Lag1, Lead1, Lag2, Lead2, Lag3, Lead3, Lag4, Lead4, Lag5, Lead5), na.rm = TRUE)
        )
    
    select(baseline_max_temp, Zone, Station, Doy = doy, Max_temp.Baseline = base_max_temp5)
}

max_temp_baseline <- get_max_temp_baseline(baseline_stations, max_temp, baseline_range[1], baseline_range[2])

get_max_temp_anamolies <- function(baseline_stations, max_temp, max_temp_baseline) {
    
    baseline_anomolies <- 
        max_temp %>% 
        mutate(Doy = yday(Date)) %>%
        inner_join(max_temp_baseline, by=c("Zone", "Station", "Doy")) %>%
        mutate(Max_temp.Anomoly = Max_temp - Max_temp.Baseline, Year = year(Date)) %>%
        select(Zone, Station, Date, Year, Doy, Max_temp, Max_temp.Source, Max_temp.Baseline, Max_temp.Anomoly)
}

baseline_anomolies_year <- 
    baseline_anomolies %>% 
    group_by(Zone, Station, Year) %>%
    summarise(Max_temp.Anomoly = mean(Max_temp.Anomoly))



baseline_anomolies_combined <- 
    baseline_anomolies %>% 
    group_by(Year) %>%
    summarise(Max_temp.Anomoly = mean(Max_temp.Anomoly))


ggplot(baseline_anomolies_combined, aes(x = Year, y = Max_temp.Anomoly)) + geom_line()

baseline_anomolies %>% 
    ggplot(aes(x = Date.Julian, y = Max_temp.Anomoly)) + 
    geom_line() + facet_wrap(~ Station)

baseline_anomolies %>% 
    filter(Station == "66062") %>%
    ggplot(aes(x = Date, y = Max_temp.Anomoly)) + 
    geom_line() + facet_wrap(~ Station)


baseline_max_temp <- function(stations, start_year, end_year) {
    
}

# Make a vector of country names

# Call the vector in `borders()`

map_aus <- borders("world", regions = "Australia",  fill = "grey90", colour = "black")

ggplot(baseline_stations) +
    map_aus +
    geom_point(aes(x = Longitude, y = Latitude, color = MinYear), na.rm=TRUE) +
    xlim(112, 155) +  # Set x axis limits, xlim(min, max)
    ylim(-44, -10) +  # Set y axis limits
    coord_quickmap() +  # Define aspect ratio of the map, so it doesn't get stretched when resizing
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude")

#library(tidyverse)
#library(reshape2)
#library(akima)
# library(gganimate)
#library(ggmap)
#library(RColorBrewer)
#library(animation)


# load_aus_map <- function() {
#     aus_bounds <- c(left = 112, bottom = -44, right = 155, top = -10 )
#     aus_map <- get_stamenmap(aus_bounds, zoom = 5, maptype = "toner-lite", where = maps_datapath())
#     aus_map
# }

# stations_Map <- function(.map, .year) {
#     year_stations <- stations %>% full_join(annual, by = "station") %>% filter(year == .year) %>% group_by(station, long,lat, year) %>% select(station, long, lat, year)
#     ggmap(.map) + geom_point(data = year_stations, aes(x = long, y = lat)) + ggtitle(paste0("Stations ", .year)) + xlab("Long") + ylab("Lat") + coord_map()
# }
# 
# rain_Plot <- function(yr) {
#     map <- vic_map
#     max <- 2000
#     min <- 0
#     breaks = seq(min, max, 500)
#     df <- stations %>% full_join(rain_annual, by = "station") %>% filter(!is.na(annual) & year == yr) %>% group_by(long, lat) %>% summarise(mean = mean(annual))
#     mat <- interp(x = df$long, y = df$lat, z = df$mean)
#     reg <- melt(mat$z, na.rm = TRUE)
#     reg$long <- mat$x[reg$Var1]
#     reg$lat <- mat$y[reg$Var2]
#     reg$year <- yr
#     
#     plot <- 
#         ggmap(map) +
#         geom_tile(data = reg, aes(x = long, y = lat, fill = value), alpha = 0.6) +
#         stat_contour(data = reg, aes(x = long, y = lat, z = value), breaks = breaks) +
#         xlab("Longitude") +
#         ylab("Latitude") +
#         labs(title=paste0("Rain ", yr)) +
#         scale_fill_gradientn("mm", colors=brewer.pal(11, "Spectral"), limits = c(min,max), breaks = breaks) +
#         coord_map()
#     plot
# }
# 
# percent_Plot <- function(yr) {
#     map <- vic_map
#     max <- 100
#     min <- 0
#     breaks = seq(min, max, 20)
#     df <- stations %>% inner_join(annual, by = "station") %>% group_by(long, lat, year) %>% summarise(annual = mean(annual)) %>% group_by(long, lat) %>% mutate(percent = percent_rank(annual) * 100) %>% filter(year == yr)
#     mat <- interp(x = df$long, y = df$lat, z = df$percent)
#     reg <- melt(mat$z, na.rm = TRUE)
#     reg$long <- mat$x[reg$Var1]
#     reg$lat <- mat$y[reg$Var2]
#     reg$year <- yr
#     
#     plot <- 
#         ggmap(map) +
#         geom_tile(data = reg, aes(x = long, y = lat, fill = value), alpha = 0.6) +
#         stat_contour(data = reg, aes(x = long, y = lat, z = value), breaks = breaks) +
#         xlab("Longitude") +
#         ylab("Latitude") +
#         labs(title=paste0("Rain Percentile ", yr)) +
#         scale_fill_gradientn("%", colors=brewer.pal(11, "Spectral"), limits = c(min,max), breaks = breaks) +
#         coord_map()
#     plot
# }
# 
# 
# rainMonthly_Plot <- function(yr,mth) {
#     map <- vic_map
#     max <- 200
#     min <- 0
#     breaks = seq(min, max, 50)
#     df <- stations %>% full_join(monthly, by = "station") %>% filter(!is.na(rain) & year == yr & month == mth) %>% group_by(long, lat) %>% summarise(mean = mean(rain))
#     mat <- interp(x = df$long, y = df$lat, z = df$mean)
#     reg <- melt(mat$z, na.rm = TRUE)
#     reg$long <- mat$x[reg$Var1]
#     reg$lat <- mat$y[reg$Var2]
#     reg$year <- yr
#     reg$month <- mth
#     
#     plot <- 
#         ggmap(map) +
#         geom_tile(data = reg, aes(x = long, y = lat, fill = value), alpha = 0.6) +
#         stat_contour(data = reg, aes(x = long, y = lat, z = value), breaks = breaks) +
#         xlab("Longitude") +
#         ylab("Latitude") +
#         labs(title=paste0("Rain ", yr, " month ", mth)) +
#         scale_fill_gradientn("mm", colors=brewer.pal(11, "Spectral"), limits = c(min,max), breaks = breaks) +
#         coord_map()
#     plot
# }
# 
# rainMonthly_video <- function(years) {
#     months <- levels(monthly$month)
#     frames <- cross2(years, months)
#     animate <- function() {
#         walk(frames, ~ print(rainMonthly_Plot(.[[1]], .[[2]])))
#     }
#     
#     #save all iterations into one GIF
#     saveVideo(animate(), interval = .2, video.name="rainmonthly.mp4")
# }
# 
# monthlyPercent_Plot <- function(yr,mth) {
#     map <- vic_map
#     max <- 100
#     min <- 0
#     breaks = seq(min, max, 20)
#     df <- 
#         stations %>% 
#         inner_join(monthly, by = "station") %>% 
#         group_by(long, lat, year, month) %>% 
#         summarise(rain = mean(rain)) %>% 
#         group_by(long, lat) %>% 
#         mutate(percent = percent_rank(rain) * 100) %>% 
#         filter(year == yr & month == mth)
#     mat <- interp(x = df$long, y = df$lat, z = df$percent)
#     reg <- melt(mat$z, na.rm = TRUE)
#     reg$long <- mat$x[reg$Var1]
#     reg$lat <- mat$y[reg$Var2]
#     reg$year <- yr
#     
#     plot <- 
#         ggmap(map) +
#         geom_tile(data = reg, aes(x = long, y = lat, fill = value), alpha = 0.6) +
#         stat_contour(data = reg, aes(x = long, y = lat, z = value), breaks = breaks) +
#         xlab("Longitude") +
#         ylab("Latitude") +
#         labs(title=paste0("Rain Percentile ", yr, " ", mth)) +
#         scale_fill_gradientn("%", colors=brewer.pal(11, "Spectral"), limits = c(min,max), breaks = breaks) +
#         coord_map()
#     plot
# }
# 
# monthlyPercent_video <- function(years) {
#     months <- levels(monthly$month)
#     frames <- cross2(years, months)
#     animate <- function() {
#         walk(frames, ~ print(monthlyPercent_Plot(.[[1]], .[[2]])))
#     }
#     
#     #save all iterations into one GIF
#     saveVideo(animate(), interval = .1, video.name="rainmonthly.mp4")
# }
# 
