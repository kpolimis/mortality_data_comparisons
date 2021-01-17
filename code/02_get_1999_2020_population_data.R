#' get census.gov population data from 1999 to 2020
# install.packages(c("here", "tidyverse"))

library(here)
library(tidyverse)

#' census urls
national_pop_1999_url = "https://www2.census.gov/programs-surveys/popest/datasets/1990-2000/intercensal/national/us-est90int-07-1999.csv"
national_pop_2000_2010_url = "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/national/us-est00int-alldata-5yr.csv"
national_2010_2020_pop_url = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv"

#' create data frames from census urls and do some column pre-processing
national_pop_1999 = read_csv(national_pop_1999_url, skip = 3,
                             col_names = c("date", "age_group", "total_population", "male_population", "female_population"),
                             col_types = cols(date  = col_date("%B %d, %Y")))
national_pop_2000_2010 = read_csv(national_pop_2000_2010_url)
national_pop_2010_2020 = read_csv(national_2010_2020_pop_url)

names(national_pop_1999)
names(national_pop_2000_2010)
names(national_pop_2010_2020)

state.name_dc_us = c(state.name, "District of Columbia", "United States")
state.abb_dc_us = c(state.abb, "DC", "US")

#' pre-process and reshape data by selecting last month of data in 1999 and all age groups
national_pop_1999_wide = national_pop_1999 %>% 
  filter(date=="1999-12-01" & age_group=="All Age") %>% 
  mutate(state_name = "United States") %>% 
  rename("pop_estimate_1999"="total_population") %>% 
  select(state_name, pop_estimate_1999) 

#' pre-process 2000 to 2010 data by filtering on age group that encompasses the entire population
national_pop_2000_2010_filtered = national_pop_2000_2010 %>%
  group_by(year) %>% 
  filter(AGEGRP==0) %>% 
  rename("value"="TOT_POP") %>% 
  mutate(state_name="United States", variable = paste("pop_estimate", year, sep="_")) %>%
  slice(1) %>%
  ungroup() %>%
  select(state_name, variable, value)

#' use reshape library to transform 2000 to 2010 census data from long to wide format
national_pop_2000_2010_wide = reshape2::dcast(national_pop_2000_2010_filtered,
                                              state_name  ~ variable,
                                              value.var = "value", fun.aggregate = sum)

#' subset 2010 to 2020 data to include on US data and rename columns to match 1999 to 2010 data
#' subset will be in wide form
national_pop_2010_2020_wide = national_pop_2010_2020 %>%
  filter(NAME %in% "United States") %>%
  rename("state_fips" = "STATE",  "state_name" = "NAME", "census_estimate_2010" = "CENSUS2010POP",
         "base_estimate_2010" = "ESTIMATESBASE2010", "pop_estimate_2010" = "POPESTIMATE2010",
         "pop_estimate_2011" = "POPESTIMATE2011", "pop_estimate_2012" = "POPESTIMATE2012", 
         "pop_estimate_2013" = "POPESTIMATE2013", "pop_estimate_2014" = "POPESTIMATE2014",
         "pop_estimate_2015" = "POPESTIMATE2015", "pop_estimate_2016" = "POPESTIMATE2016",
         "pop_estimate_2017" = "POPESTIMATE2017", "pop_estimate_2018" = "POPESTIMATE2018",
         "pop_estimate_2019" = "POPESTIMATE2019", "pop_estimate_2020" = "POPESTIMATE2020") %>% 
  select(state_fips:pop_estimate_2020)

#' combine all 3 census population data sets into one wide data set of 1999 to 2020 population data. 
#' select only columns available across all 3 population data sets
national_1999_2020_pop_wide = cbind(national_pop_1999_wide,
                                    national_pop_2000_2010_wide %>% select(-state_name),
                                    national_pop_2010_2020_wide %>% select(-state_fips:-pop_estimate_2010))
#' save population data
write_csv(national_1999_2020_pop_wide, here("data/national_population_1999_2020.csv"))
