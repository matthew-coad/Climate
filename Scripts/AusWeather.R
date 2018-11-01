library(tidyverse)
library(rlang)
library(lubridate)
library(rvest)
library(emc)

## --------------------------------------------------
## Prep
## --------------------------------------------------

aus_root_directory <- climate_datapath("AUS")

aus_data_directory <- function(repos) file.path(aus_root_directory, repos)

## --------------------------------------------------
## Australian Weather Station Lists
## --------------------------------------------------

aus_station_info <- function(name, file_name, source_url) {
    list(
        name = name,
        file_name = file_name,
        source_url = source_url,
        file_path = file.path(aus_root_directory, file_name)
    )
}

aus_download_station = function(info) {
    
    if (!file.exists(info$file_path)) {
        cat("----------------------------------------------\n")
        cat("Downloading ", info$name," Stations\n")
        download.file(info$source_url, info$file_path, mode="wb")
        cat("----------------------------------------------\n")
    }
}

aus_memorise_stations <- function(info) {
    if (file.exists(info$file_path))
        file.remove(info$file_path)
    aus_download_station(info)
}

aus_load_stations <- function(info) {
    
    path <- info$file_path
    stationText <- read_file(path)
    allLines <- strsplit(stationText, "\r\n")[[1]]
    
    code <- c(3,7)
    name <- c(9,48)
    lat <- c(49,57)
    long <- c(59,67)
    startDate <- c(69,76)
    endDate <- c(78,85)
    year <- c(87,92)
    percent <- c(95,97)
    aws <- c(101,101)
    
    firstStation <- which(substr(allLines,1,10) == "----------") + 1
    lastStation = first(which(trimws(allLines) == "") - 1)
    stationLines = allLines[firstStation:lastStation]
    
    stationCols <- function(col) trimws(substr(stationLines, col[1], col[2]))
    
    codes = stationCols(code)
    names = stationCols(name)
    lats = stationCols(lat) %>% map_dbl(as.double)
    longs = stationCols(long) %>% map_dbl(as.double)
    years = stationCols(year) %>% map_int(as.integer)
    
    tibble(
        Zone = "AUS",
        Station = codes,
        Name = names,
        Latitude = lats,
        Longitude = longs
    )
}

## --------------------------------------------------
## Australian Weather Observations
## --------------------------------------------------

## Raw Observation Data

aus_observation_info <- function(station_info, name, file_name, lookup_url, internal_name, loader, validator, observation_variable, units) {
    list(
        station_info = station_info,
        name = name,
        file_name = file_name,
        lookup_url = lookup_url,
        internal_name = internal_name,
        loader = loader,
        validator = validator,
        observation_variable = observation_variable,
        Units = units
    )
}

aus_observation_directory <- function(info) {
    aus_data_directory(info$name)
}

aus_observation_file_name <- function(info, code) {
    info$file_name(code)
}

aus_observation_file_path <- function(info, code) {
    file.path(aus_observation_directory(info), aus_observation_file_name(info, code))
}

aus_observation_lookup_url <- function(info, code) {
    info$lookup_url(code)
}

aus_observation_internal_name <- function(info, code) {
    info$internal_name(code)
}

aus_observation_url = function(info, code) {
    lookup_url <- aus_observation_lookup_url(info, code)
    page <- read_html(lookup_url)
    contentLinks <- page %>% html_node("#content-block") %>% html_nodes("a")
    idx <- which(contentLinks %>% html_text()  == "All years of data")
    href <- contentLinks[idx] %>% html_attr("href")
    url <- paste0("http://www.bom.gov.au", href)
    url
}

aus_download_observation <- function(code, info, i = 1, l = 1) {
    
    directory <- aus_observation_directory(info)
    if (!dir.exists(directory)) {
        dir.create(directory, recursive = TRUE)
    }
    path <- aus_observation_file_path(info, code)
    if (!file.exists(path)) {
        
        cat("----------------------------------------------\n")
        cat("Downloading Station", code,  info$name, i, "of", l, "\n")
        url <- tryCatch(aus_observation_url(info, code), error = function(e) NULL)
        if (!is.null(url)) {
            result <- tryCatch(
                { 
                    download.file(url, path, mode="wb", quiet = TRUE)
                    list(code = code, status = "downloaded", error = NULL)
                }, 
                error = function(e) {
                    list(code = code, status = "failed", error = e)
                }
            )
        }
        else {
            result <- list(code = code, status = "lookup failed", error = NULL)
        }
        cat("----------------------------------------------\n")
    }
    else {
        result <- list(code = code, status = "present", error = NULL)
    }
    result
}

aus_download_observations <- function(stations, info) {
    l <- nrow(stations)
    ix <- 1:l
    run <- function(i) {
        station <- stations[i,]
        code <- station$Station
        aus_download_observation(code, info, i, l)
    }
    results <- ix %>% map(run)
    results <- results %>% map(~ list(code = .$code, status = .$status, error = ifelse(!is.null(.$error), conditionMessage(.$error), "") ) ) %>% bind_rows()
    results$name = info$name
    results
}

aus_observations <- function(stations, observation_info) {
    
    station_observations <- function(station) {
        file_path <- aus_observation_file_path(observation_info, station)
        internal_name <- aus_observation_internal_name(observation_info, station)
        
        if (!file.exists(file_path)) {
            return (NULL)
        }
        
        observation_info$loader(file_path, internal_name)
    }
    stations %>% purrr::map_dfr(station_observations)
}

aus_validate_observations <- function(stations, observation_info) {
    
    station_validation <- function(station) {
        file_path <- aus_observation_file_path(observation_info, station)
        internal_name <- aus_observation_internal_name(observation_info, station)
        observation_info$validator(station, file_path, internal_name)
    }
    stations %>% purrr::map_dfr(station_validation)
}

aus_variable_summariser <- function(variable, prefix = rlang::quo_name(variable)) {
    summary_namer <- function(extension) if_else(!is.null(prefix), paste0(prefix, ".", extension), extension)
    
    summarisers <- list2(
        !! summary_namer("N") := quo(length(!! variable)),
        !! summary_namer("N_NA") := quo(sum(is.na(!! variable))),
        !! summary_namer("N_Present") := quo(sum(!is.na(!! variable))),
        !! summary_namer("N_Zero") := quo(sum(!! variable == 0, na.rm = TRUE)),
        !! summary_namer("N_NonZero") := quo(sum(!! variable != 0, na.rm = TRUE)),
        
        !! summary_namer("NearZeroVar") := quo(length(caret::nearZeroVar(!! variable)) == 1),
        
        !! summary_namer("Min") := quo(min(!! variable, na.rm = TRUE)),
        !! summary_namer("Min") := quo(min(!! variable, na.rm = TRUE)),
        !! summary_namer("Mean") := quo(mean(!! variable, na.rm = TRUE)),
        !! summary_namer("Median") := quo(median(!! variable, na.rm = TRUE)),
        !! summary_namer("Max") := quo(max(!! variable, na.rm = TRUE)),
        !! summary_namer("FirstQ") := quo(quantile(!! variable, probs = c(0.25), na.rm = TRUE)),
        !! summary_namer("ThirdQ") := quo(quantile(!! variable, probs = c(0.75), na.rm = TRUE)),
        !! summary_namer("SD") := quo(sd(!! variable, na.rm = TRUE)),
        !! summary_namer("Skew") := quo(e1071::skewness(!! variable, na.rm = TRUE))
    )
    summarisers
}

aus_observation_summary <- function(station, observation_info) {

    all_stations <- aus_load_stations(observation_info$station_info)
    observation_variable <- observation_info$observation_variable
    station_df <- tibble(Zone = "AUS", Station = station)
    variable_df <- tibble(variable = rlang::quo_name(observation_info$observation_variable), units = observation_info$Units)
    
    validation_df <- aus_validate_observations(station, observation_info) %>% select(Status = status, Error = error)

    station_count <- all_stations %>% filter(Station == station) %>% nrow()
    if (station_count > 1) {
        validation_df$Status <- "Duplicate"
    }
    observations_available <- validation_df$Status == "Good"
    
    if (observations_available) {
        observations <- aus_observations(station, observation_info)
        years <- observations %>% mutate(Year = year(Date)) %>% pull(Year) %>% unique()
        year_summary <- tibble(N_Year = length(years), Min_Year = min(years), Max_Year = max(years))
    }
    else {
        observations <- tibble(Date = lubridate::as_date(integer(0)), !! observation_variable := double()) 
        years <- integer(0)
        year_summary <- tibble(N_Year = 0, Min_Year = na_int, Max_Year = na_int)
    }
    
    summarisers <- aus_variable_summariser(observation_info$observation_variable, prefix = NULL)
    all_summary <- observations %>% summarise(!!! summarisers)
    decadal_summary <- observations %>% mutate(Decade = floor(year(Date) / 10) * 10) %>% group_by(Decade) %>% summarise(!!! summarisers)
    yearly_summary <- observations %>% mutate(Year = year(Date)) %>% group_by(Year) %>% summarise(!!! summarisers)

    bind_cols(
        station_df,
        variable_df, 
        validation_df, 
        year_summary, 
        tibble(
           All = list(all_summary), 
           Decadal = list(decadal_summary), 
           Yearly = list(yearly_summary)
       ))
}

## Memorisation

aus_observation_memorise_directory <- function(info) {
    file.path(aus_data_directory(info$name), "Memorise")
}

aus_clean_memorise_directory <- function(info) {
    path <- file.path(aus_data_directory(info$name), "Memorise")
    if (dir.exists(path)) {
        unlink(path, recursive = TRUE)
    }
    dir.create(path, recursive = TRUE)
    invisible()
}

aus_memorise <- function(eval, observation_info, memo_name) {
    
    memorise_path <- file.path(aus_observation_memorise_directory(observation_info), paste0(memo_name, ".RDS"))
    
    if (file.exists(memorise_path)) {
        return (readRDS(memorise_path))
    }
    
    result <- eval()
    saveRDS(result, memorise_path)
    result
}

## Memorised Summaries

aus_observation_summaries <- function(observation_info) {
    
    station_info <- observation_info$station_info
    summariser <- function() {
        stations <- aus_load_stations(station_info)
        summaries <- stations %>% pull(Station) %>% map_dfr(function(station) aus_observation_summary(station, observation_info))
        summaries
    }
    
    aus_memorise(summariser, observation_info, "Observation_Summaries")
}

aus_clean_info <- function(name, observation_info, longitude_lim = c(112, 155), latitude_lim = c(-44, -10), 
                           yearly_n_target = 360, yearly_present_target = yearly_n_target / 4, 
                           clean_years_target = 30) {
    list(
        name = name,
        observation_info = observation_info,
        longitude_lim = longitude_lim,
        latitude_lim = latitude_lim,
        yearly_n_target = yearly_n_target,
        yearly_present_target = yearly_present_target,
        clean_years_target =  clean_years_target
    )
}

# Summary of information needed to clean observations
aus_observation_clean_summary <- function(stations, clean_info) {
    
    # clean_info <- aus_rain_clean_info 

    observation_info <- clean_info$observation_info
    station_info <- observation_info$station_info
    station_df <- aus_load_stations(station_info) %>% filter(Station %in% stations) %>% distinct(Zone, Station, .keep_all = TRUE)
    summaries <- aus_observation_summaries(observation_info) %>% filter(Station %in% stations) %>% distinct(Zone, Station, .keep_all = TRUE)
    summaries <- station_df %>% inner_join(summaries, by = c("Zone", "Station"))
    summary_all <- summaries %>% unnest(All)
    data_available <- summary_all$Status == "Good"
    longitude_inside <- data_available & between(summary_all$Longitude, clean_info$longitude_lim[1], clean_info$longitude_lim[2])
    latitude_inside <- data_available & between(summary_all$Latitude, clean_info$latitude_lim[1], clean_info$latitude_lim[2])
    present_available <- summary_all$N_NonZero > 0
    
    yearly_summary <-
        summaries[data_available, ] %>%
        unnest(Yearly) %>%
        mutate(N_Target = clean_info$yearly_n_target) %>%
        mutate(N_Target_Met = N > N_Target) %>%
        mutate(N_Present_Target = clean_info$yearly_present_target) %>%
        mutate(N_Present_Target_Met = N_Present > N_Present_Target) %>%
        mutate(Year_Clean = N_Target_Met & N_Present_Target_Met) %>%
        select(Zone, Station, Year, Year_Clean,
               N, N_Target, N_Target_Met, N_NA, N_Present, N_Present_Target, N_Present_Target_Met, 
               N_Zero, N_NonZero, Min, Mean, Max, FirstQ, Median, ThirdQ, SD, Skew)
    yearly_clean_summary <- 
        yearly_summary %>%
        group_by(Zone, Station) %>%
        summarise(
            Clean_Years = sum(Year_Clean),
            Clean_Year_Target = clean_info$clean_years_target,
            Clean_Year_Target_Met = Clean_Years > Clean_Year_Target
        )
    
    yearly_nested_summary <- yearly_summary %>% group_by(Zone, Station) %>% nest(.key = Clean_Yearly)
    
    clean_summary <-
        summaries %>%
        mutate(
            Data_Available = data_available,
            Longitude_Target_Met = longitude_inside,
            Latitude_Target_Met = latitude_inside,
            Is_Present_Target_Met = present_available
        ) %>%
        left_join(yearly_clean_summary, by=c("Zone", "Station")) %>%
        mutate(
            Clean = Data_Available & Longitude_Target_Met & Latitude_Target_Met & Is_Present_Target_Met & Clean_Year_Target_Met
        ) %>%
        left_join(yearly_nested_summary, by = c("Zone", "Station")) %>%
        select(
            Zone, Station, Name, Latitude, Longitude, Variable = variable, Clean, Data_Available, 
            N_Year, Clean_Years, Min_Year, Max_Year, All, Decadal, Yearly, Clean_Yearly, everything(), -units, -Status, -Error)
    clean_summary
}

aus_clean_observations <- function(stations, clean_info) {
    
    observation_info <- clean_info$observation_info
    observations <- aus_observations(stations, observation_info)
    
    clean_summary <- aus_observation_clean_summary(stations, clean_info)
    clean_years <- clean_summary %>% filter(Clean) %>% unnest(Clean_Yearly) %>% filter(Year_Clean) %>% select(Zone, Station, Year)
    clean_observations <- observations %>%
        mutate(Year = year(Date)) %>%
        inner_join(clean_years, by = c("Zone", "Station", "Year")) %>%
        select(-Year)
    clean_observations
}


## --------------------------------------------------
## Observation Types
## --------------------------------------------------

#Bureau of Meteorology product IDCJMC0014.                                       Produced: 07 Oct 2018
#Australian stations measuring rainfall
aus_rainfall_station_info <- aus_station_info("rainfall", "alphaAUS_136.txt", "http://www.bom.gov.au/climate/data/lists_by_element/alphaAUS_136.txt")

# Bureau of Meteorology product IDCJMC0014.                                       Produced: 07 Oct 2018
# Australian stations measuring maximum air temperature
aus_temperature_station_info <- aus_station_info("temperature", "alphaAUS_122.txt", "http://www.bom.gov.au/climate/data/lists_by_element/alphaAUS_122.txt")

aus_rainfall_observations_info <- aus_observation_info(
    aus_rainfall_station_info,
    "rainfall",  
    function(code) paste0("Station_", code, ".zip"),
    function(code) paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=136&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", code),
    function(code) paste0("IDCJAC0009_", code, "_1800_Data.csv"),
    function(file_path, internal_name) {
        colTypes <- cols(
            `Product code` = col_character(),
            `Bureau of Meteorology station number` = col_character(),
            Year = col_integer(),
            Month = col_character(),
            Day = col_character(),
            `Rainfall amount (millimetres)` = col_double(),
            `Period over which rainfall was measured (days)` = col_integer(),
            Quality = col_character()
        )        
        raw_observations <- read_csv(unz(file_path, internal_name), col_types = colTypes)
        observations <- 
            raw_observations %>% 
            mutate(
                Zone = "AUS",
                Rainfall.Source = 'BOM',
                Date = make_date(Year, Month, Day)
            ) %>%
            select(
                Zone,
                Station = `Bureau of Meteorology station number`,
                Date,
                Rainfall = `Rainfall amount (millimetres)`,
                Rainfall.Measurement_Days  = `Period over which rainfall was measured (days)`,
                Rainfall.Source
            )
        observations
                
    },
    function(code, file_path, internal_name) {
        
        if (!file.exists(file_path)) {
            return (tibble(status = "Data Missing", error = "", rows = 0 ))
        }
        
        colTypes <- cols(
            `Product code` = col_character(),
            `Bureau of Meteorology station number` = col_character(),
            Year = col_integer(),
            Month = col_character(),
            Day = col_character(),
            `Rainfall amount (millimetres)` = col_double(),
            `Period over which rainfall was measured (days)` = col_integer(),
            Quality = col_character()
        )        
        error <- NULL
        result <- tryCatch(
            suppressWarnings(read_csv(unz(file_path, internal_name), col_types = colTypes)), 
            error = function(e) { 
                error <<- e 
            })
        
        if (!is.null(error)) {
            return (tibble(status = "Read Error", error = conditionMessage(error), rows = 0))
        }
        
        if (nrow(result) == 0) {
            return (tibble(status = "No Data", error = "", rows = 0))
        }
        return (tibble(status = "Good", error = "", rows = nrow(result)))
        
    },
    sym("Rainfall"),
    "millimetres"
)

aus_maxtemp_observations_info <- aus_observation_info(
    aus_temperature_station_info,
    "maxtemp",  
    function(code) paste0("IDCJAC0010_", code, "_1800.zip"),
    function(code) paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=122&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", code),
    function(code) paste0("IDCJAC0010_", code, "_1800_Data.csv"),
    function(file_path, internal_name) {
        colTypes <- cols(
            `Product code` = col_character(),
            `Bureau of Meteorology station number` = col_character(),
            Year = col_integer(),
            Month = col_integer(),
            Day = col_integer(),
            `Maximum temperature (Degree C)` = col_double(),
            `Days of accumulation of maximum temperature` = col_integer(),
            Quality = col_character()
        )   
        raw_observations <- read_csv(unz(file_path, internal_name), col_types = colTypes)
        observations <- 
            raw_observations %>% 
            mutate(
                Zone = "AUS",
                Max_temp.Source = 'BOM',
                Date = make_date(Year, Month, Day)
            ) %>%
            select(
                Zone,
                Station = `Bureau of Meteorology station number`,
                Date,
                Max_temp = `Maximum temperature (Degree C)`,
                Max_temp.Measurement_Days  = `Days of accumulation of maximum temperature`,
                Max_temp.Source
            )
        observations
    },
    function(code, file_path, internal_name) {
        
        if (!file.exists(file_path)) {
            return (tibble(status = "Data Missing", error = "", rows = 0 ))
        }
        
        colTypes <- cols(
            `Product code` = col_character(),
            `Bureau of Meteorology station number` = col_character(),
            Year = col_integer(),
            Month = col_integer(),
            Day = col_integer(),
            `Maximum temperature (Degree C)` = col_double(),
            `Days of accumulation of maximum temperature` = col_integer(),
            Quality = col_character()
        )        
        error <- NULL
        result <- tryCatch(
            suppressWarnings(read_csv(unz(file_path, internal_name), col_types = colTypes)), 
            error = function(e) { 
                error <<- e 
            })
        
        if (!is.null(error)) {
            return (tibble(status = "Read Error", error = conditionMessage(error), rows = 0))
        }
        
        if (nrow(result) == 0) {
            return (tibble(status = "No Data", error = "", rows = 0))
        }
        return (tibble(status = "Good", error = "", rows = nrow(result)))
    },
    sym("Max_temp"),
    "Degree C"

)

aus_mintemp_observations_info <- aus_observation_info(
    aus_temperature_station_info,
    "mintemp",  
    function(code) paste0("IDCJAC0011_", code, "_1800.zip"),
    function(code) paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=123&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", code),
    function(code) paste0("IDCJAC0011_", code, "_1800_Data.csv"),
    function(file_path, internal_name) {
        colTypes <- cols(
            `Product code` = col_character(),
            `Bureau of Meteorology station number` = col_character(),
            Year = col_integer(),
            Month = col_integer(),
            Day = col_integer(),
            `Minimum temperature (Degree C)` = col_double(),
            `Days of accumulation of minimum temperature` = col_integer(),
            Quality = col_character()
        )
        raw_observations <- read_csv(unz(file_path, internal_name), col_types = colTypes)
        
        observations <- 
            raw_observations %>% 
            mutate(
                Zone = "AUS",
                Min_temp.Source = 'BOM',
                Date = make_date(Year, Month, Day)
            ) %>%
            select(
                Zone,
                Station = `Bureau of Meteorology station number`,
                Date,
                Min_temp = `Minimum temperature (Degree C)`,
                Min_temp.Measurement_Days  = `Days of accumulation of minimum temperature`,
                Min_temp.Source
            )
        observations
        
    },
    function(code, file_path, internal_name) {
        
        if (!file.exists(file_path)) {
            return (tibble(status = "Data Missing", error = "", rows = 0 ))
        }
        
        colTypes <- cols(
            `Product code` = col_character(),
            `Bureau of Meteorology station number` = col_character(),
            Year = col_integer(),
            Month = col_integer(),
            Day = col_integer(),
            `Minimum temperature (Degree C)` = col_double(),
            `Days of accumulation of minimum temperature` = col_integer(),
            Quality = col_character()
        )
        error <- NULL
        result <- tryCatch(
            suppressWarnings(read_csv(unz(file_path, internal_name), col_types = colTypes)), 
            error = function(e) { 
                error <<- e 
            })
        
        if (!is.null(error)) {
            return (tibble(status = "Read Error", error = conditionMessage(error), rows = 0))
        }
        
        if (nrow(result) == 0) {
            return (tibble(status = "No Data", error = "", rows = 0))
        }
        return (tibble(status = "Good", error = "", rows = nrow(result)))
    },
    sym("Min_temp"),
    "Degree C"
    
)

## -----------------------------------------------------
## Memorise data
## -----------------------------------------------------

aus_update_temperature <- function() {
    
    # Discard memorised information
    aus_clean_memorise_directory(aus_maxtemp_observations_info)
    aus_clean_memorise_directory(aus_mintemp_observations_info)
    
    # Always download station list
    aus_memorise_stations(aus_temperature_station_info)
    
    # Download any missing files
    stations <- aus_load_stations(aus_temperature_station_info)
    aus_download_observations(stations, aus_maxtemp_observations_info)
    aus_download_observations(stations, aus_mintemp_observations_info)
    
    # Request observation summaries to ensure they are cached
    aus_observation_summaries(aus_maxtemp_observations_info)
    aus_observation_summaries(aus_mintemp_observations_info)
}

aus_update_rainfall <- function() {
    
    # Discard memorised information
    aus_clean_memorise_directory(aus_rainfall_station_info)
    
    # Always download station list
    aus_memorise_stations(aus_rainfall_station_info)
    
    # Download any missing files
    stations <- aus_load_stations(aus_rainfall_station_info)
    aus_download_observations(stations, aus_rainfall_observations_info)
    
    # Request observation summaries to ensure they are cached
    aus_observation_summaries(aus_rainfall_observations_info)
}

