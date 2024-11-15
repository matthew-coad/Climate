# --- Australian Station Analysis ---

# Generates the data needed to do the weather stations quality analysis

#' Generates the rainfall station quality statistics.
aus_station_quality_summary <- function() {
    complete_stations <- aus_load_stations(aus_rainfall_station_info)
    station_summary <- 
        aus_observation_summaries(aus_rainfall_observations_info) %>%
        unnest(All) %>%
        mutate('N %'= (N / (N_Year * 365)) * 100, `NA %` = (N_NA / N) * 100, 'Zero %' = (N_Zero / (N_Zero + N_NonZero)) * 100) %>%
        inner_join(select(complete_stations, Zone, Station, Latitude, Longitude) , by = c("Zone","Station")) %>%
        select(-variable, -units, -Decadal, -Yearly)
    station_summary
}

# --- Australian Rainfall Analysis ---

# Generates the data needed for the Australian Rainfall Analysis

#' Determine rainfall analysis stations
#' 
#' Returns a list of stations that are considered "Active" during the baseline period.
#' To be active a stations must have mostly complete data for the specified number of clean
#' years.
#' 
#' @param baseline_start Start year of the baseline period
#' @param  baseline_end End year of the baseline period
#' @param clean_years Minimum number of years of observations needed
#' 
#' @return Stations data frame
aus_rainfall_stations <- function(baseline_start, baseline_end, clean_years) {
    valuation_info <- aus_rainfall_valuation_info
    valuation_info$baseline_start <- baseline_start
    valuation_info$baseline_end <- baseline_end
    valuation_info$clean_years <- clean_years
    
    stations <- aus_baseline_stations(valuation_info)
    stations
}

#' Load rainfall observations
#' 
#' @param stations Vector of stations to load observations for
#' 
#' @return Observations data frame
aus_rainfall_observations <- function(stations) {
    aus_clean_observations(stations, aus_rainfall_clean_info)
}

#' Load memorised data needed for the rainfall analysis
#' 
#' @param baseline_start Start year of the baseline period
#' @param  baseline_end End year of the baseline period
#' @param clean_years Minimum number of years of observations needed
#' 
#' @return List containing rainfall stations and observations
aus_memorised_rainfall_data <- function(baseline_start, baseline_end, clean_years) {

    load_data <- function() {
        message("Loading Stations")
        stations <- aus_rainfall_stations(baseline_start, baseline_end, clean_years)
        message("Loading Observations")
        observations <- aus_clean_observations(stations$Station, aus_rainfall_clean_info)
        message("Data load complete")
        list(
            stations = stations,
            observations = observations
        )
    }
    
    # Name to memorise the data under
    memo_name <- paste0("RainfallBins v", aus_rainfall_version, " base from ", baseline_start, " to ", baseline_end, " min ", clean_years)
    
    # And load it!
    memorised_data <- aus_memorise(load_data, aus_rainfall_observations_info, memo_name)
    memorised_data
}

#' Given a set of rainfall observations, converts them into monthly observations.
#' 
#' Also returns the observations for the prior 3,6, 12 and 24 months.
#' 
#' @return A data frame containing aggregated totals for each station per month.
aus_rainfall_monthly_observations <- function(observations) {
    
    # Set the period divisions. Originally they could be changed but this analysis now pretty much requires
    # them to be years divided into months.
    major_period <- aus_year_period()
    minor_period <- aus_monthly_period()
    
    # Aggregate the data per period
    incomplete_period_observations <-
        observations %>% 
        mutate(Major_Period = !! major_period, Minor_Period = !! minor_period) %>%
        group_by(Zone, Station, Major_Period, Minor_Period) %>%
        summarise(
            Rainfall = sum(Rainfall, na.rm = TRUE),
            Rainfall.Source = first(Rainfall.Source)
        ) %>%
        ungroup() %>%
        arrange(Zone, Station, Major_Period, Minor_Period)
    
    # Work out some stats, per station per minor period (IE Months)
    period_summary <-
        incomplete_period_observations %>% 
        group_by(Zone, Station, Minor_Period) %>%
        summarise(
            Rainfall.Mean = mean(Rainfall, na.rm = TRUE),
            Rainfall.Median = median(Rainfall, na.rm = TRUE),
            Rainfall.Min = min(Rainfall, na.rm = TRUE),
            Rainfall.Max = max(Rainfall, na.rm = TRUE),
            Rainfall.IQR = IQR(Rainfall, na.rm = TRUE)
        )
    
    # What we have so far may contain gaps which creates isssues when
    # we want to aggregate lagged data. We deal with this by imputing
    # missing data with the median for that minor period.
    
    # Then for each month we work out the clean total plus the prior 3, 6, 12 and 24 months totals
    # total
    period_observations <-
        incomplete_period_observations %>% 
        group_by(Zone, Station) %>%
        complete(Major_Period = full_seq(Major_Period, 1), Minor_Period = 1:12) %>%
        ungroup() %>%
        inner_join(period_summary, by = c("Zone", "Station", "Minor_Period")) %>%
        mutate(
            Date = make_date(Major_Period, Minor_Period),
            Rainfall.Original = Rainfall,
            Rainfall = if_else(!is.na(Rainfall.Original), Rainfall.Original, Rainfall.Median)) %>%
        select(Zone, Station, Date, Major_Period, Minor_Period, Rainfall, Rainfall.Original, Rainfall.Source) %>%
        arrange(Zone, Station, Major_Period, Minor_Period) %>%
        mutate(
            Rainfall.L3 = Rainfall + lag(Rainfall, 1) + lag(Rainfall, 2),
            Rainfall.L6 = Rainfall.L3 + lag(Rainfall, 3) + lag(Rainfall, 4) + lag(Rainfall, 5),
            Rainfall.L12 = Rainfall.L6 + lag(Rainfall, 6) + lag(Rainfall, 7) + lag(Rainfall, 8) + 
                lag(Rainfall, 9) + lag(Rainfall, 10)  + lag(Rainfall, 11),
            Rainfall.L24 = 
                Rainfall.L12 + 
                lag(Rainfall, 12) + lag(Rainfall, 13) + lag(Rainfall, 14) + lag(Rainfall, 15) + lag(Rainfall, 16) + lag(Rainfall, 17) + 
                lag(Rainfall, 18) + lag(Rainfall, 19) + lag(Rainfall, 20) + lag(Rainfall, 21) + lag(Rainfall, 22) + lag(Rainfall, 23) 
        )
    period_observations
}

#' Given the aggregated periodic observations, workout the baseline statistics for each station.
#' 
#' Works out a variety of statistics for each station for each month of the year. Includes the min, max
#' quartiles. Also works out quantiles for a configurable number of bins which are used to work out ratings.
#' 
#' So for each station for each month you know things like whats the median rainfall for the month,
#' the last 3 months.
#' 
#' Those stats are also worked out for each of the lagged observations. So you get what are the deciles for the last month, 
#' last 3 months and so on.
aus_rainfall_baselines <- function(period_observations, baseline_start, baseline_end, n_quantile) {
    

    # Make an open ended quantile by setting the first break to zero and the last break to infinity
    open_quantile <- function(x) {
        x[1] <- 0
        x[length(x)] <- Inf
        x
    }
    
    # For all period observations in the baseline period work out summar stats for the current 3, 12 and 24 months
    baselines <-
        period_observations %>% 
        filter(Major_Period >= baseline_start, Major_Period <= baseline_end) %>%
        group_by(Zone, Station, Minor_Period) %>%
        summarise(
            Rainfall.Max = max(Rainfall),
            Rainfall.Min = min(Rainfall),
            Rainfall.1st = quantile(Rainfall, 0.25),
            Rainfall.Median = median(Rainfall),
            Rainfall.3rd = quantile(Rainfall, 0.75),
            Rainfall.Quantiles = list(quantile(Rainfall, (0:n_quantile) / n_quantile, na.rm = TRUE)),
            Rainfall.OpenQuantiles = list(open_quantile(quantile(Rainfall, (0:n_quantile) / n_quantile, na.rm = TRUE))),
            
            Rainfall.L3.Max = max(Rainfall.L3),
            Rainfall.L3.Min = min(Rainfall.L3),
            Rainfall.L3.1st = quantile(Rainfall.L3, 0.25),
            Rainfall.L3.Median = median(Rainfall.L3),
            Rainfall.L3.3rd = quantile(Rainfall.L3, 0.75),
            Rainfall.L3.Quantiles = list(quantile(Rainfall.L3, (0:n_quantile) / n_quantile, na.rm = TRUE)),
            Rainfall.L3.OpenQuantiles = list(open_quantile(quantile(Rainfall.L3, (0:n_quantile) / n_quantile, na.rm = TRUE))),
            
            Rainfall.L12.Max = max(Rainfall.L12),
            Rainfall.L12.Min = min(Rainfall.L12),
            Rainfall.L12.1st = quantile(Rainfall.L12, 0.25),
            Rainfall.L12.Median = median(Rainfall.L12),
            Rainfall.L12.3rd = quantile(Rainfall.L12, 0.75),
            Rainfall.L12.Quantiles = list(quantile(Rainfall.L12, (0:n_quantile) / n_quantile, na.rm = TRUE)),
            Rainfall.L12.OpenQuantiles = list(open_quantile(quantile(Rainfall.L12, (0:n_quantile) / n_quantile, na.rm = TRUE))),
            
            Rainfall.L24.Max = max(Rainfall.L24),
            Rainfall.L24.Min = min(Rainfall.L24),
            Rainfall.L24.1st = quantile(Rainfall.L24, 0.25),
            Rainfall.L24.Median = median(Rainfall.L24),
            Rainfall.L24.3rd = quantile(Rainfall.L24, 0.75),
            Rainfall.L24.Quantiles = list(quantile(Rainfall.L24, (0:n_quantile) / n_quantile, na.rm = TRUE)),
            Rainfall.L24.OpenQuantiles = list(open_quantile(quantile(Rainfall.L24, (0:n_quantile) / n_quantile, na.rm = TRUE)))
            
        ) 
    baselines
}

#' Given monthly observations and baselines, work out the valuations for
#' each monthly observation
aus_rainfall_monthly_valuations <- function(monthly_observations, baselines, n_quantile) {
    
    quantile_offset <- ceiling(n_quantile/ 2)
    
    valuations <-
        monthly_observations %>% 
        group_by(Zone, Station, Minor_Period) %>%
        inner_join(baselines, by=c("Zone", "Station", "Minor_Period")) %>%
        mutate(Date = make_date(Major_Period, Minor_Period)) %>%
        mutate(Rainfall.Rating = findInterval(Rainfall, first(Rainfall.OpenQuantiles)) - quantile_offset) %>%
        mutate(Rainfall.L3.Rating = findInterval(Rainfall.L3, first(Rainfall.L3.OpenQuantiles)) - quantile_offset) %>%
        mutate(Rainfall.L12.Rating = findInterval(Rainfall.L12, first(Rainfall.L12.OpenQuantiles)) - quantile_offset) %>%
        mutate(Rainfall.L24.Rating = findInterval(Rainfall.L24, first(Rainfall.L24.OpenQuantiles)) - quantile_offset) %>%
        ungroup() %>%
        select(
            Zone, Station, Major_Period, Minor_Period, Date,
            Rainfall.L12.Median,
            Rainfall, Rainfall.Rating,
            Rainfall.L3, Rainfall.L3.Rating,
            Rainfall.L12, Rainfall.L12.Rating,
            Rainfall.L24, Rainfall.L24.Rating, 
            Rainfall.Source)
    valuations
}

#' Given a station list and the monthly valuations work, group each station into a "region".
#' 
#' @return Data frame that gives a region ID for each station
aus_rainfall_regions <- function(stations, monthly_valuations, 
                                 region_baseline_start, region_baseline_years, region_baseline_minor_period, distance_weighting, centers) {
    
    # Region generation options
    region_baseline_end <- region_baseline_start + region_baseline_years - 1
    
    # Spread the rainfall during the baseline period into a row per station.
    # Scale all the values with a weighting for disance
    spread_valuations <-
        monthly_valuations %>% 
        filter(Major_Period >= region_baseline_start, Major_Period <= region_baseline_end, Minor_Period == region_baseline_minor_period) %>%
        inner_join(stations, by=c("Zone", "Station")) %>%
        mutate(Latitude = scale(Latitude) * distance_weighting, Longitude = scale(Longitude) * distance_weighting) %>%
        mutate(Rating_Value = scale(Rainfall.L3)) %>%
        select(Zone, Station, Latitude, Longitude, Rainfall = Major_Period, Rating_Value) %>% 
        spread(key = Rainfall, value = Rating_Value, sep = ".") %>%
        ungroup()
    
    # Function that evaluates regions for a given number of Kmeans centers
    set.seed(101)
    region_kmeans <- 
        spread_valuations %>% 
        select(-Zone, -Station) %>% 
        kmeans(centers)
    
    regions <- spread_valuations
    regions$RegionID <- region_kmeans$cluster
    regions <- regions %>% select(Zone, Station, RegionID)
    regions
}


