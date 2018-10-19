source("~/MCD/Projects/Climate/Scripts/emc.R")
source("~/MCD/Projects/Climate/Scripts/Settings.R")
source("~/MCD/Projects/Climate/Scripts/Temperature.R")
source("~/MCD/Projects/Climate/Scripts/AusWeather.R")
source("~/MCD/Projects/Climate/Scripts/Utility.R")

maxtemp_summaries <- aus_observation_summaries(aus_maxtemp_observations_info)
test_summaries <- maxtemp_summaries %>% filter(Status == "Good") %>% slice(1:9)
test_summary <- test_summaries[1,]
test_station <- test_summary$Station

test_observations <- test_summaries$Station %>% map_dfr(~ aus_observations(., aus_maxtemp_observations_info))
test_valuation_info <- aus_valuation_info("Test", aus_maxtemp_observations_info)

test_initial_valuations <- aus_valuation(test_observations, test_valuation_info)
test_baselines <- aus_valuation_baselines(test_observations, test_valuation_info)

test_valuations <- aus_valuation(test_observations, test_valuation_info, baselines = test_baselines)

