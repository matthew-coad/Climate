## -----------------------------------------------------
## Aus Climage evaluation 
## -----------------------------------------------------

aus_year_period <- function(variable = sym("Date")) quo(year(!! variable ))
aus_daily_period <- function(variable = sym("Date")) quo(yday(!! variable ))
aus_monthly_period <- function(variable = sym("Date")) quo(month(!! variable ))
aus_yearly_period <- function(variable = sym("Date")) 0
aus_baseline_start <- 1951
aus_baseline_end <- 1980

aus_valuation_info <- function(
    name,
    clean_info, 
    baseline_start = aus_baseline_start,
    baseline_end = aus_baseline_end,
    clean_years = baseline_end - baseline_start + 1,
    major_period = aus_year_period(), 
    minor_period = aus_daily_period(),
    is_daily = TRUE,
    input_variables = list(clean_info$observation_info$observation_variable), 
    mutators = list(),
    summarisers = list(), 
    output_variable = clean_info$observation_info$observation_variable,
    scale = FALSE)
{
    observation_info <- clean_info$observation_info
    list(
        name = name,
        observation_info = observation_info,
        clean_info = clean_info,
        baseline_start = baseline_start,
        baseline_end = baseline_end,
        clean_years = clean_years,
        major_period = major_period,
        minor_period = minor_period,
        is_daily = is_daily,
        input_variables = input_variables,
        mutators = mutators,
        summarisers = summarisers,
        output_variable = output_variable,
        anamoly_variable = rlang::sym(sprintf("%s.Anamoly", rlang::quo_name(observation_info$observation_variable))),
        scale = scale
    )
}

aus_baseline_stations <- function(valuation_info) {
    
    # valuation_info <- aus_maxtemp_valuation_info
    
    observation_info <- valuation_info$observation_info
    clean_info <- valuation_info$clean_info
    start_year <- valuation_info$baseline_start
    end_year <- valuation_info$baseline_end
    clean_years <- valuation_info$clean_years
    station_info <- observation_info$station_info
    stations <- aus_load_stations(station_info)
    station_summaries <- aus_observation_clean_summary(stations$Station, clean_info) %>% filter(Clean)
    station_yearly_summary <- station_summaries %>% unnest(Clean_Yearly) %>% filter(Year >= start_year, Year <= end_year, Year_Clean)
    
    station_qualifying_years <-
        station_yearly_summary %>%
        group_by(Zone, Station) %>%
        summarise(N_Years = n())
    
    qualifying_stations <- station_qualifying_years %>% filter(N_Years >= clean_years)
    
    baseline_stations <- stations %>% inner_join(qualifying_stations, by = c("Zone", "Station"))
    baseline_stations
}

aus_valuations <- function(observations, valuation_info, baselines = NULL) {
    
    # Prototype Start
    # observations <- sample_observations
    # valuation_info <- sample_valuation_info
    # station <- test_station
    # valuation_info <- aus_maxtemp_valuation_info
    
    observation_info <- valuation_info$observation_info
    
    scale <- valuation_info$scale
    has_baselines <- !is.null(baselines)
    is_daily <- valuation_info$is_daily
    
    input_variables <- valuation_info$input_variables
    input_variable_names <- input_variables %>% map_chr(~ rlang::quo_name(.))
    observation_mutators <- valuation_info$mutators
    observation_summarisers <- valuation_info$summarisers
    output_variable <- valuation_info$output_variable
    output_variable_name <- rlang::quo_name(output_variable)
    anamoly_variable <- valuation_info$anamoly_variable
    
    
    if (!is_daily) {
        date_mutators <- list2()
        date_summarisers <- list2(
            Median_Date = quo(as_date(floor((as.integer(max(Date)) + as.integer(min(Date))) / 2))),
            Min_Date = quo(min(Date)),
            Max_Date = quo(max(Date))
        )
    }
    else {
        date_mutators <- list2(
            Median_Date = quo(Date),
            Min_Date = quo(Date),
            Max_Date = quo(Date)
        )
        date_summarisers <- list2()
    }
    
    if (!has_baselines) {
        baselines_names <- c()
        anomoly_mutators <- list()
    }
    else {
        
        baselines_names <- c("Baseline.Mean", "Baseline.SD")
        if (scale) {
            anomoly_mutators <- list2(
                !! anamoly_variable := quo((!! output_variable - Baseline.Mean) / Baseline.SD)
            )
        }
        else {
            anomoly_mutators <- list2(
                !! anamoly_variable := quo(!! output_variable - Baseline.Mean)
            )
        }
    }
    
    valuations <- 
        observations %>% 
        mutate(Major_Period = !! valuation_info$major_period, Minor_Period = !! valuation_info$minor_period) %>%
        mutate(!!! observation_mutators, !!! date_mutators)
    
    summarisers <- append(observation_summarisers, date_summarisers)
    if (length(summarisers) > 0) {
        valuations <- 
            valuations %>%
            group_by(Zone, Station, Major_Period, Minor_Period) %>%
            summarise(!!! summarisers) %>%
            ungroup()
    }
    if (!is.null(baselines)) {
        valuations <- 
            valuations %>%
            left_join(baselines, by = c("Zone", "Station", "Minor_Period"))
    }    
    if (length(anomoly_mutators) > 0) {
        valuations <- 
            valuations %>% 
            mutate(!!! anomoly_mutators)
    }
    
    variables <- c(
        "Zone",
        "Station",
        "Major_Period",
        "Minor_Period",
        input_variable_names,
        names(observation_mutators),
        names(observation_summarisers),
        baselines_names,
        names(anomoly_mutators),
        names(date_mutators),
        names(date_summarisers)
    )
    valuations <- valuations %>% select(!! variables)
    valuations
}

aus_valuation_baselines <- function(observations, valuation_info) {
    
    baseline_observations <-
        observations %>% 
        mutate(Major_Period = !! valuation_info$major_period, Minor_Period = !! valuation_info$minor_period) %>%
        filter(Major_Period >= valuation_info$baseline_start & Major_Period <= valuation_info$baseline_end)
    
    valuations <- aus_valuations(baseline_observations, valuation_info)
    
    output_variable <- valuation_info$output_variable
    
    valuation_baselines <- valuations %>%
        group_by(Zone,Station,Minor_Period) %>%
        summarise(
            Baseline.N = sum(!is.na(!! output_variable)),
            Baseline.Mean = mean(!! output_variable, na.rm = TRUE),
            Baseline.SD = sd(!! output_variable, na.rm = TRUE)
        )
    valuation_baselines
}

## -----------------------------------------------------
## Temperature valuations
## -----------------------------------------------------

aus_maxtemp_clean_info <- aus_clean_info("Max_temp", aus_maxtemp_observations_info)
aus_mintemp_clean_info <- aus_clean_info("Min_temp", aus_mintemp_observations_info)
aus_maxtemp_valuation_info <- aus_valuation_info("Max_temp", aus_maxtemp_clean_info)

## -----------------------------------------------------
## Rainfall valuations
## -----------------------------------------------------


# Rainfall memorisation version
aus_rainfall_version <- 1
aus_rainfall_clean_info <- aus_clean_info("Rainfall", aus_rainfall_observations_info, yearly_present_target = 350, clean_years_target = 50)

aus_rainfall_valuation_info <- aus_valuation_info(
    "Rainfall", aus_rainfall_clean_info, baseline_start = aus_baseline_start, baseline_end = aus_baseline_end, 
    minor_period = aus_monthly_period(),
    input_variables = list(),
    mutators = list(
    ),
    summarisers = list2(
        Monthly_Rainfall = quo(sum(Rainfall, na.rm = TRUE)),
        Sqrt_Monthly_Rainfall = quo(sqrt(Monthly_Rainfall+0.1))
    ),
    output_variable = sym("Sqrt_Monthly_Rainfall"),
    scale = TRUE)



