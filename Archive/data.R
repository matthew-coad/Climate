library(tidyverse)
library(rvest)


## -----------------------------------------------------
## General data path resources
## -----------------------------------------------------

data_path = function(name) file.path("~", "MCD", "Projects", "VicRainfall", "Data", name)

data_ensurePath = function(name) {
  dataFolder <- file.path(getwd(), "Data")
  if (!dir.exists(dataFolder))
    dir.create(dataFolder)
  path <- data_path(name)
  if (!dir.exists(path))
    dir.create(path)
  path
}
data_filePath = function(name, file) file.path(data_path(name), file)

## -----------------------------------------------------
## Rainfall data
## -----------------------------------------------------

rain_path = data_path("rainfall")

rain_stationFileName = function(code) paste0("Station_", code, ".zip")
rain_stationPath = function(code) data_filePath("rainfall", rain_stationFileName(code))

rain_getStationUrl = function(code) {
    rainUrl <- paste0("http://www.bom.gov.au/jsp/ncc/cdio/wData/wdata?p_nccObsCode=139&p_display_type=dataFile&p_stn_num=", code)
    rainPage <- read_html(rainUrl)
    contentLinks <- rainPage %>% html_node("#content-block") %>% html_nodes("a")
    idx <- which(contentLinks %>% html_text()  == "All years of data")
    href <- contentLinks[idx] %>% html_attr("href")
    url <- paste0("http://www.bom.gov.au", href)
    url
}

rain_downloadStation = function(code) {
    url <- rain_getStationUrl(code)
    path <- rain_stationPath(code)
    download.file(url, path, mode="wb")
}

rain_downloadStations = function(codes) {
    l <- length(codes)
    ix <- 1:l
    run <- function(i) {
        station <- stations[i,]
        code <- station$code
        cat("----------------------------------------------\n")
        cat("Downloading Station", code, i, "of", l, "\n")
        rain_downloadStation(code)
        cat("----------------------------------------------\n")
    }
    ix %>% walk(run)
}

rain_data12FileName = function(code) paste0("IDCJAC0001_", code, "_Data12.csv")

rain_loadData12Table = function(code) {
    path <- rain_stationPath(code)
    fileName <- rain_data12FileName(code)
    colTypes <- cols(
        `Product code` = col_character(),
        `Station Number` = col_character(),
        Year = col_integer(),
        Jan = col_character(),
        Feb = col_character(),
        Mar = col_character(),
        Apr = col_character(),
        May = col_character(),
        Jun = col_character(),
        Jul = col_character(),
        Aug = col_character(),
        Sep = col_character(),
        Oct = col_character(),
        Nov = col_character(),
        Dec = col_character(),
        Annual = col_character()
    )
    tryCatch(suppressWarnings(read_csv(unz(path, fileName), col_types = colTypes)), error = function(e) NULL)
}

rain_loadAnnual = function(stations) {
    codes <- stations$station
    data12 <- 
        codes %>% 
        map(rain_loadData12Table) %>% 
        map_df(~ .)
    data12 %>% 
        mutate(annual = suppressWarnings(as.numeric(Annual, n))) %>%
        filter(!is.na(annual)) %>%
        select(
            product = `Product code`, 
            station = `Station Number`, 
            year = Year,
            annual = annual
        )
}

rain_loadMonthly = function(stations) {
    codes <- stations$station
    data12 <- 
        codes %>% 
        map(rain_loadData12Table) %>% 
        map_df(~ .)
    months <- colnames(data12)[which(colnames(data12) == "Jan"):which(colnames(data12) == "Dec")]
    data12 %>%
        gather(month, rain, Jan:Dec) %>%
        mutate(rain = suppressWarnings(as.numeric(rain, n))) %>%
        mutate(month = factor(month, levels=months, ordered=T)) %>%
        filter(!is.na(rain)) %>%
        select(
            product = `Product code`, 
            station = `Station Number`, 
            year = Year,
            month,
            rain = rain
        )
}

## -----------------------------------------------------
## Weather Station data
## -----------------------------------------------------

stations_dataPath <- data_path("Stations")
stations_vicRainfallFileName <- "Vic_Rainfall.txt"
stations_vicRainfallPath <- data_filePath("Stations", stations_vicRainfallFileName)

stations_load <- function() {
    
    stationText <- read_file(stations_vicRainfallPath)
    allLines <- strsplit(stationText, "\r\r\n")[[1]]
    
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
        station = codes,
        name = names,
        lat = lats,
        long = longs,
        years = years
    )
}

## -----------------------------------------------------
## Southern Oscillation Index Data
## -----------------------------------------------------

SOI_name <- "SOI"
SOI_path <- function() data_path(SOI_name)
SOI_filePath <- function() data_filePath(SOI_name, "SOI_Monthly.html")

SOI_download <- function() {
    url <- "ftp://ftp.bom.gov.au/anon/home/ncc/www/sco/soi/soiplaintext.html"  
    data_ensurePath(SOI_name)
    filePath <- SOI_filePath()
    download.file(url, filePath, mode="wb")
}

SOI_load <- function() {
    filePath <- SOI_filePath()
    page <- read_html(filePath)
    text <- page %>% html_node("body") %>% html_node("pre") %>% html_text()
    col_names <- c("year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    col_widths <- fwf_widths(c(9, rep(8,11), 7), col_names=col_names)
    col_types <- cols(col_integer(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number())
    df <- read_fwf(text, col_widths, col_types = col_types, skip  = 2)
    months <- col_names[which(col_names == "Jan"):which(col_names == "Dec")]
    df %>%
        gather(month, index, Jan:Dec) %>%
        mutate(month = factor(month, levels=months, ordered=T))
}
