
co2_datapath <- function(filename) climate_datapath(filename)


# Credit: Institute for Atmospheric and Climate Science (IAC) at Eidgenössische Technische Hochschule in Zürich, Switzerland.
# Link: ftp://data.iac.ethz.ch/CMIP6/input4MIPs/UoM/GHGConc/CMIP/yr/atmos/UoM-CMIP-1-1-0/GHGConc/gr3-GMNHSH/v20160701/mole_fraction_of_carbon_dioxide_in_air_input4MIPs_GHGConcentrations_CMIP_UoM-CMIP-1-1-0_gr3-GMNHSH_0000-2014.csv

co2_iac_annual_0To2014_datapath <- function() {
    co2_datapath("mole_fraction_of_carbon_dioxide_in_air_input4MIPs_GHGConcentrations_CMIP_UoM-CMIP-1-1-0_gr3-GMNHSH_0000-2014.csv")
}

get_co2_iac_annual_0To2014 <- function() {
    file_path <- co2_iac_annual_0To2014_datapath()
    col_types <- cols(
        year = col_integer(),
        data_mean_global = col_double(),
        data_mean_nh = col_double(),
        data_mean_sh = col_double()
    )
    co2_raw <- read_csv(file_path, col_types = col_types)
    result <- tibble(
        CO2.Source = 'IAC',
        Year = co2_raw$year, 
        CO2.Global = co2_raw$data_mean_global, 
        CO2.NHem = co2_raw$data_mean_nh,
        CO2.SHem = co2_raw$data_mean_sh
    )
    result
}

co2_mauna_annual_datapath <- function() {
    co2_datapath("co2_annmean_mlo.txt")
}

get_co2_mauna_annual_1959toPresent <- function() {
    file_path <- co2_mauna_annual_datapath()
    raw_lines <- read_lines(file_path)
    data_lines <- raw_lines[str_sub(raw_lines,1,1) != '#']
    years <- as.integer(str_sub(data_lines, 3, 6))
    co2_global <- as.double(str_sub(data_lines, 7, 15))
    result <- tibble(
        CO2.Source = 'MAUNA',
        Year = years,
        CO2.Global = co2_global
    )
    result
}

get_co2_annual <- function() {
    iac <- get_co2_iac_annual_0To2014()
    mauna <- get_co2_mauna_annual_1959toPresent()
    result <- iac %>% anti_join(mauna, by="Year") %>% bind_rows(mauna) %>% arrange(Year)
    result
}


