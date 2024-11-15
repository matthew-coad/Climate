library(lubridate)

sunspots_datapath <- function(filename) climate_datapath(filename)


# --------------------------
# Daily total sunspot number
# --------------------------

# File: FSN_d_tot_V2.0.csv

# Source: http://sidc.be/silso/datafiles#total

# Data description:
#     Daily total sunspot number derived by the 
#     formula: R= Ns + 10 * Ng, with Ns the number of spots and Ng the number of groups counted over the entire solar disk.
#     
#     No daily data are provided before 1818 because daily observations become too sparse in earlier years. Therefore, R. Wolf only 
#     compiled monthly means and yearly means for all years before 1818.
#     
#     In the TXT and CSV files, the missing values are marked by -1 (valid Sunspot Number are always positive).
#     
#     New scale:
#
#      The conventional 0.6 Zürich scale factor is not used anymore and A. Wolfer (Wolf's successor) is now defining 
#      the scale of the entire series. This puts the Sunspot Number at the scale of raw modern counts, instead of reducing 
#      it to the level of early counts by R. Wolf.
#                                                                                     
#      Error values:
#      Those values correspond to the standard deviation of raw numbers provided by all stations. Before 1981, the
#      errors are estimated with the help of an auto-regressive model based on the Poissonian distribution of actual Sunspot 
#      Numbers. From 1981 onwards, the error value is the actual standard deviation of the sample of raw observations 
#      used to compute the daily value.
#
#      The standard error of the daily Sunspot Number can be computed by:
#      sigma/sqrt(N) where sigma is the listed standard deviation and N the number of observations for the day.
#
#      Before 1981, the number of observations is set to 1, as the Sunspot Number was then essentially the raw 
#      Wolf number from the Zürich Observatory.

# Contents:
#     Column 1-3: Gregorian calendar date
# - Year
# - Month
# - Day
# Column 4: Date in fraction of year
# Column 5: Daily total sunspot number. A value of -1 indicates that no number is available for that day (missing value).
# Column 6: Daily standard deviation of the input sunspot numbers from individual stations.
# Column 7: Number of observations used to compute the daily value.
# Column 8: Definitive/provisional indicator. A blank indicates that the value is definitive. 
# A '*' symbol indicates that the value is still provisional and is subject to a possible revision (Usually the last 3 to 6 months)

sunspots_sils_daily_1818toNow_datapath <- function() {
    sunspots_datapath("SN_d_tot_V2.0.csv")
}

get_sunspots_sils_daily_1818toNow <- function() {
    file_path <- sunspots_sils_daily_1818toNow_datapath()
    col_names <- c("Year", "Month", "Day", "DateDecimal", "sunspots", "sunspot_sd", "obs_count", "definite")
    col_types <- cols(
        Year = col_integer(),
        Month = col_character(),
        Day = col_character(),
        DateDecimal = col_double(),
        sunspots = col_character(),
        sunspot_sd = col_character(),
        obs_count = col_character(),
        definite = col_integer()
    )
    sunspots_raw <- read_delim(file_path, col_names = col_names, col_types = col_types, delim  = ";")
    result <- tibble(
        Date = make_date(as.integer(sunspots_raw$Year), as.integer(sunspots_raw$Month), as.integer(sunspots_raw$Day)),
        Sunspots = ifelse(as.integer(sunspots_raw$sunspots) != -1, as.integer(sunspots_raw$sunspots), NA),
        Sunspots.source = 'SILS',
        Sunspots.sd = ifelse(as.double(sunspots_raw$sunspot_sd) != -1.0, as.double(sunspots_raw$sunspot_sd), NA),
        Sunspots.observations = as.integer(sunspots_raw$obs_count),
        Sunspots.definite = as.integer(sunspots_raw$definite)
    )
    result
}

get_sunspots_daily <- function() {
    get_sunspots_sils_daily_1818toNow()
}

get_sunspots_monthly <- function() {
    get_sunspots_sils_daily_1818toNow() %>% 
    group_by(Year, Month) %>% 
    summarise(SS.Count = mean(SS.Count, na.rm = TRUE), SS.Count.Sd = mean(SS.Count.Sd, na.rm = TRUE), SS.Observations = mean(SS.Observations))
}

get_sunspots_yearly <- function() {
    get_sunspots_sils_daily_1818toNow() %>% 
        group_by(Year) %>% 
        summarise(SS.Count = mean(SS.Count, na.rm = TRUE), SS.Count.Sd = mean(SS.Count.Sd, na.rm = TRUE), SS.Observations = mean(SS.Observations))
}

