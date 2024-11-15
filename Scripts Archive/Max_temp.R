library(lubridate)

source("~/MCD/Projects/Climate/Scripts/AusWeather.R")

## Get GISTEMP, Zonal annual means
get_max_temp_AUS <- function() {
    aus_load_combined_observations(aus_maxtemp_observations_info)
}

get_stations_AUS <- function() {
    aus_load_stations(aus_temperature_station_info)
}

get_baseline_stations <- function(stations, observations, variable, start_year, end_year, qualifying_ratio = 0.90) {
    
    qualifying_days <- ((end_year - start_year) * 365) * qualifying_ratio

    station_qualifying_days <- 
        observations %>% 
        filter(!is.na(!! variable), year(Date) >= start_year, year(Date) <= end_year) %>%
        group_by(Zone, Station) %>% 
        summarise(n_days = n())
    
    qualifying_stations <- station_qualifying_days %>% filter(n_days >= qualifying_days)
    
    baseline_stations <- stations %>% inner_join(qualifying_stations, by = c("Zone", "Station"))
    baseline_stations
}

get_baseline_observations <- function(stations, observations, variable, baseline_start, baseline_end, n_lag = 5, n_lead = 5) {
    variable_name <- rlang::quo_name(variable)
    if (n_lag > 0) {
        lag_mutators <- seq_len(n_lag) %>% map(~ quo(lag(!! variable, .))) %>% set_names(paste0(variable_name, ".Lag", seq_len(n_lag)))
    }
    else {
        lag_mutators <- list()
    }
    if (n_lead > 0) {
        lead_mutators <- seq_len(n_lead) %>% map(~ quo(lead(!! variable, .))) %>% set_names(paste0(variable_name, ".Lead", seq_len(n_lead)))
    }
    else {
        lead_mutators <- list()
    }
    baseline_observations <- 
        observations %>% 
        inner_join(select(stations, Zone, Station), by = c("Zone", "Station")) %>% 
        mutate(Year = year(Date), Doy = yday(Date)) %>%
        arrange(Zone, Station, Date) %>%
        mutate(
            !!! lag_mutators,
            !!! lead_mutators
        ) %>%
        dplyr::filter(Year >= baseline_start, Year <= baseline_end) %>%
        select(Zone, Station, Date, Year, Doy, everything())
    baseline_observations
}

get_observation_baselines <- function(stations, observations, variable, baseline_start, baseline_end, n_lag = 5, n_lead = 5) {
    
    baseline_observations <- 
        get_baseline_observations(
            stations, 
            observations, 
            variable, 
            baseline_start, 
            baseline_end, 
            n_lag, 
            n_lead)
    
    variable_name <- rlang::quo_name(variable)
    baseline_variable_name <- paste0(variable_name, ".Baseline")
    input_selectors <- variable_name
    if (n_lag > 0) {
        input_selectors <- c(input_selectors, paste0(variable_name, ".Lag", seq_len(n_lag)))
    }
    if (n_lead > 0) {
        input_selectors <- c(input_selectors, paste0(variable_name, ".Lead", seq_len(n_lead)))
    }
    input_selectors <- input_selectors %>% map(~ sym(.))
    
    mean_quo <- quo(mean(c(!!! input_selectors), na.rm = TRUE))

    baseline_observations %>%
        group_by(Zone, Station, Doy) %>%
        summarise(!! baseline_variable_name := !! mean_quo)
}

get_observation_anamolies <- function(stations, observations, variable, observation_baselines) {
    variable_name <- rlang::quo_name(variable)
    baseline_variable <- rlang::sym(paste0(variable_name, ".Baseline"))
    anamoly_variable <- rlang::sym(paste0(variable_name, ".Anamoly"))
    
    observations %>%
        mutate(Year = year(Date), Doy = yday(Date)) %>%
        inner_join(observation_baselines, by=c("Zone", "Station", "Doy")) %>%
        mutate(!! anamoly_variable := !! quo(!! variable - !! baseline_variable))
}

# sample_start_year <- 1900
# sample_end_year <- 1920
# sample_idp <- 5
# 
# sample_year_anamoly <- 
#     max_temp_yearly_anamoly %>% 
#     filter(Year >= sample_start_year, Year <= sample_end_year) %>% 
#     inner_join(baseline_stations, by=c("Zone", "Station"))
# 
# abs_max_anamoly <- ceiling(max(abs(sample_year_anamoly$Max_temp.Anamoly))*10)/10
# 
# get_observation_grid <- function(stations, observation_summary, variable) {
#     
#     variable_name <- rlang::quo_text(variable)
#     station_summary <- stations %>% inner_join(observation_summary, by = c("Zone", "Station"))
#     
#     grid <- expand.grid(Longitude = rlang::seq2(112, 155), Latitude = rlang::seq2(-44, -10))
#     sp::coordinates(grid) <- ~Longitude+Latitude
#     
#     sp::coordinates(station_summary) <- ~Longitude+Latitude
#     station_summary.idw <- suppressMessages(gstat::idw(Max_temp.Anamoly ~ 1, station_summary, newdata = grid, idp = sample_idp))
#     
#     observation_grid <-station_summary.idw %>% as.data.frame() %>% as_tibble() %>% select(Longitude, Latitude, !! variable_name := var1.pred)
#     observation_grid
# }
# 
# sample_year_anamoly_grid <- get_observation_grid(
#     baseline_stations, 
#     max_temp_yearly_anamoly %>% filter(Year >= sample_start_year, Year <= sample_end_year),
#     quo(Max_temp.Anamoly))
# 
# map_aus_dark <- 
#     with_nameSpace(
#         "maps", 
#         ggplot2::borders("world", regions = "Australia",  fill = "DarkGrey", colour = "black")
#     )
# 
# sample_year_anamoly_plot <-
#     sample_year_anamoly %>%
#     ggplot() +
#     map_aus_dark +
#     geom_point(aes(x = Longitude, y = Latitude, color = Max_temp.Anamoly), na.rm=TRUE) +
#     geom_tile(data = sample_year_anamoly_grid, aes(x = Longitude, y = Latitude, fill = Max_temp.Anamoly), alpha = 0.4) +
#     xlim(112, 155) +  # Set x axis limits, xlim(min, max)
#     ylim(-44, -10) +  # Set y axis limits
#     coord_quickmap() +  # Define aspect ratio of the map, so it doesn't get stretched when resizing
#     labs(
#         title = paste0("Temperature Anamoly from ", sample_start_year, " to "
#         ),
#         caption = "Source: 'BOM'",
#         x = "",
#         y = ""
#     ) +
#     theme_bw() +
#     theme(
#         axis.text.y = element_blank(),
#         axis.text.x = element_blank()
#     ) + 
#     scale_colour_distiller(type = "div", palette="RdBu", direction = -1, limits = c(-abs_max_anamoly,abs_max_anamoly)) +
#     scale_fill_distiller(type = "div", palette="RdBu", direction = -1, limits = c(-abs_max_anamoly,abs_max_anamoly))
# sample_year_anamoly_plot
# 
# 
