#' compare mortality data from nchs/census with social media
# install.packages(c("here", "httr", "reshape2", "tidyverse"))

library(here)
library(reshape2)
library(lubridate)
library(tidyverse)

#' read in programmatically created mortality and population data
#' read in social media mortality mortality and population data
mortality_time_series_national = read_csv(here("output/mortality_time_series_national.csv"))
social_media_mortality_data = read_csv(here("data/social_media_mortality_data.csv"))

mortality_time_series_national$source = "CDC & Census"
social_media_mortality_data$source = "Social Media"

glimpse(mortality_time_series_national)
glimpse(social_media_mortality_data)

#' combine our curated mortality data with social media mortality data into one long mortality dataset.
mortality_comparison_data_long = rbind(mortality_time_series_national, social_media_mortality_data)

#' create a wide combined mortality dataset and add three comparison variables: 
#' (1) the mortality rate
#' (2) the mortality rate lag 
#' (3) the rate of change for the mortality rate
mortality_comparison_data_wide = reshape2::dcast(mortality_comparison_data_long, state_name + year + source ~ variable) %>%
  group_by(source) %>%
  arrange(source) %>% 
  mutate(mortality_rate = round((all_deaths/pop_estimate)*100000),
         mortality_rate_lag = lag(mortality_rate, order_by = year),
         mortality_rate_roc = (mortality_rate - mortality_rate_lag)/mortality_rate_lag)

start_date = min(mortality_comparison_data_wide$year)
end_date = max(mortality_comparison_data_wide$year)

us_mortality_rate_plot = ggplot(mortality_comparison_data_wide,
                           aes(year, mortality_rate, group=source, colour=source)) +
  geom_line(size=1) + 
  labs(x = "Date", y = "Mortality Rate per 100,000 Residents",
       title = "US Mortality Rate: Time Series",
       subtitle = sprintf("Comparison between Government data and Social Media data from: %s to %s", start_date, end_date),
       colour = "Source") + 
  # geom_hline(aes(yintercept = mean(mortality_rate, na.rm=TRUE)))  +
  # theme_bw() + 
  ylim(750,1000)

us_mortality_rate_roc_plot = ggplot(mortality_comparison_data_wide,
                           aes(year, mortality_rate_roc, group=source, colour=source)) +
  geom_line(size=1) + 
  labs(x = "Date", y = "Rate of Change",
       title = "US Mortality Rate: Rate of Change Time Series",
       subtitle = sprintf("Comparison between Government data and Social Media data from: %s to %s", start_date, end_date),
       colour = "Source") + 
  # geom_hline(aes(yintercept = mean(mortality_rate_roc, na.rm=TRUE)))  + 
  # theme_bw() + 
  ylim(-.05, .15)

us_mortality_rate_facet_plot = us_mortality_rate_plot + facet_grid(rows = vars(source))
us_mortality_rate_roc_facet_plot = us_mortality_rate_roc_plot + facet_grid(rows = vars(source))

ggsave(here("output/us_mortality_rate_plot.png"), us_mortality_rate_plot, width=10.67, height=6, dpi=120)
ggsave(here("output/us_mortality_rate_roc_plot.png"), us_mortality_rate_roc_plot, width=10.67, height=6, dpi=120)

ggsave(here("output/us_mortality_rate_facet_plot.png"), us_mortality_rate_facet_plot, width=10.67, height=6, dpi=120)
ggsave(here("output/us_mortality_rate_roc_facet_plot.png"), us_mortality_rate_roc_facet_plot, width=10.67, height=6, dpi=120)

#' compare the social media mortality data and the mortality data we created by cohort and period
cohort_period_comparisons =  mortality_comparison_data_wide %>%
  mutate(period = ifelse(year<2020,"1999-2019", "2020"),
         cohort = ifelse(year>=1999 & year<2005, "1999-2004",
                         ifelse(year>=2005 & year<2010, "2005-2009",
                                ifelse(year>=2010 & year<2015, "2010-2014",
                                       ifelse(year>=2015 & year<2020, "2015-2019", "2020"))))) %>% 
  group_by(source, cohort) %>% 
  mutate(cohort_mortality_rate = round(mean(mortality_rate, na.rm = TRUE)),
         cohort_mortality_rate_roc = round(mean(mortality_rate_roc, na.rm = TRUE),3)) %>%
  ungroup() %>% 
  group_by(source, period) %>% 
  mutate(period_mortality_rate = round(mean(mortality_rate, na.rm = TRUE)),
         period_mortality_rate_roc = round(mean(mortality_rate_roc, na.rm = TRUE), 3)) %>%
  select(source, cohort, period, cohort_mortality_rate, period_mortality_rate,
         cohort_mortality_rate_roc, period_mortality_rate_roc) %>% 
  distinct()

#'save workspace
ifelse(!dir.exists(file.path("rdata")), dir.create(file.path("rdata")), FALSE)
save.image(here("rdata/compare_mortality_data_sources.RData"), compress = TRUE)
