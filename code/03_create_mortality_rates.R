#' create mortality rates from national center for health statistics mortality data and census population data
# install.packages(c("here", "reshape2", "tidyverse", "data.table"))

library(here)
library(reshape2)
library(tidyverse)
library(data.table)

#' read in Census population data
#' read in NCHS mortality data
national_population_1999_2020 =  read_csv(here("data/national_population_1999_2020.csv"))
yearly_deaths_by_state_1999_2020 = read_csv(here("data/yearly_deaths_by_state_1999_2020.csv"))

names(national_population_1999_2020)
names(yearly_deaths_by_state_1999_2020)

#' reshape mortality and population datasets into long datasets
yearly_deaths_by_state_1999_2020_long = reshape2::melt(yearly_deaths_by_state_1999_2020, id.vars = c("state_name", "year"))
national_population_1999_2020_long = reshape2::melt(national_population_1999_2020, id.vars = c("state_name"))
national_population_1999_2020_long$year = mapply(FUN= function(variable) strsplit(as.character(variable),"estimate_")[[1]][2], national_population_1999_2020_long$variable)
national_population_1999_2020_long$variable = mapply(FUN= function(variable) substr(as.character(variable),1,12), national_population_1999_2020_long$variable)

#' create national mortality and population data
mortality_time_series_national = rbindlist(list(national_population_1999_2020_long,
                                                yearly_deaths_by_state_1999_2020_long),
                                           use.names=TRUE) %>% filter(state_name=="United States")

unique(mortality_time_series_national$variable)

#' reshape national mortality and population data into a wide mortality/population dataset from 1999 to 2020
#' create mortality rates per 100,000 for each year
#' also create mortality rate of change metric 
us_mortality_data_1999_2020 = reshape2::dcast(mortality_time_series_national, state_name + year ~ variable) %>%
  arrange(year) %>% 
  mutate(mortality_rate = round((all_deaths/pop_estimate)*100000),
         mortality_rate_lag = lag(mortality_rate, order_by = year),
         mortality_rate_roc = (mortality_rate - mortality_rate_lag)/mortality_rate_lag)

#' save national mortality and population dataset and reshaped dataset with mortality statistics
ifelse(!dir.exists(file.path("output")), dir.create(file.path("output")), FALSE)
write_csv(mortality_time_series_national, here("output/mortality_time_series_national.csv"))
write_csv(us_mortality_data_1999_2020, here("output/us_mortality_data_1999_2020.csv"))
