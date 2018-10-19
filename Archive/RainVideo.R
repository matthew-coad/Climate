
## -----------------------------------------------------------------
## Script that generates the victorian rainfall by percentiles video
## -----------------------------------------------------------------

source('~/MCD/Projects/VicRainfall/data.R')
source('~/MCD/Projects/VicRainfall/videos.r')

stations = stations_load()
rain_annual = rain_loadAnnual(stations)
rain_monthly = rain_loadMonthly(stations)
SOI = SOI_load()
vic_map = vicMap_load()

rain_video()
