
library(lubridate)

irradiance_datapath <- function(filename) climate_datapath(filename)


get_noaa_irradiance_daily <- function() {
    irradiance_path <- irradiance_datapath("composite_d25_07_0310a.dat")
    irradiance_lines <- read_lines(irradiance_path)
    irradiance_lines <- irradiance_lines[44:length(irradiance_lines)]
    irradiance_dates <- irradiance_lines %>% str_sub(2, 7) %>% ymd()
    irradiance_day <- irradiance_lines %>% str_sub(8, 16) %>% as.numeric()
    irradiance_value <- irradiance_lines %>% str_sub(17, 27) %>% as.numeric()
    irradiance_value <- ifelse(irradiance_value != -99.0, irradiance_value, NA)
    
    result <- tibble(
        Date = irradiance_dates,
        Irradiance = irradiance_value,
        Irradiance.source = 'NOAA'
    )
    result
}

get_irradiance_daily <- function() {
    get_noaa_irradiance_daily()  
} 
