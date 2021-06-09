# Libraries
library(tidyverse)
library(lubridate)

################################
## Vaccine Efficacy Estimates ##
################################

# Breakthrough Cases (from BC CDC presentation)
one_dose_cases = 1340
two_dose_cases = 120
non_vaccinated_cases = 78020

# Alternate: Calculate only > 60 Adult Cases from BC CDC Case Data (to approximate similar demographics to vaccinated)

# Load BC CDC Case Data
setwd("~/GitHub/COVID19/2021-04_BC_Covid/Shiny_App")
df_case <- readRDS("case_data.rds") %>%
           mutate(Reported_Date = as.Date(Reported_Date),
                  yr = year(as.Date(Reported_Date)),
                  mth = month(as.Date(Reported_Date)),
                  wk = week(as.Date(Reported_Date))) %>%
           mutate(year_week = as.character(ymd(paste0(yr,"-01-07")) + weeks(wk-1))) %>%
           filter(!wk %in% 53,
                  !Age_Group %in% c('Unknown'),
                  !HA %in% c('Out of Canada'))

total_cases <- df_case %>%
               filter(!Age_Group %in% c('<10', '10-19', '20-39', '30-39', '40-49', '50-59'),
                      Reported_Date %within% interval(ymd("2020-12-27"), ymd("2021-05-01"))) %>%
               count()

non_vaccinated_cases <- total_cases - one_dose_cases - two_dose_cases

# Load Covid Tracker data (to get vaccination dates)
# Data: https://covid19tracker.ca/vaccinationtracker.html

setwd("~/GitHub/COVID19/2021-04_BC_Covid")
df_vax <- readRDS("vaccine_data.rds") %>%
          mutate(date = as.Date(date)) %>%
          mutate(one_dose_days = case_when((as.Date('2021-05-01') - date - 21) > 0 ~ (as.Date('2021-05-01') - date - 21),
                                                TRUE ~ 0),
                 two_dose_days = case_when((as.Date('2021-05-01') - date - 7) > 0 ~ (as.Date('2021-05-01') - date - 7),
                                                TRUE ~ 0)) %>%
          mutate(one_dose_vax_days = as.numeric((change_vaccinations - change_vaccinated) * one_dose_days - change_vaccinated * two_dose_days),
                 two_dose_vax_days = as.numeric(change_vaccinated * two_dose_days)) %>%
          replace_na(list(one_dose_vax_days=0, two_dose_vax_days=0))

# calc total days of vaccinations
total_one_dose_vax_days = sum(df_vax$one_dose_vax_days)
total_two_dose_vax_days = sum(df_vax$two_dose_vax_days)

# calc total days of non-vaccinated
adult_pop <- sum(df_pop[!df_pop$age_group %in% c('<10', '10-19', '20-29', '30-39', '40-49', '50-59') & df_pop$HA == 'BC',"population"])
total_non_vax_days = (adult_pop * (as.numeric(as.Date('2021-05-01') - as.Date('2020-12-27'))) - total_one_dose_vax_days - total_two_dose_vax_days)
          
# Calc Case Rates
non_vax_rate = (non_vaccinated_cases / total_non_vax_days) * 1000000
one_dose_rate = (one_dose_cases / total_one_dose_vax_days) * 1000000
two_dose_rate = (two_dose_cases / total_two_dose_vax_days) * 1000000

non_vax_rate
one_dose_rate
two_dose_rate

one_dose_rate / non_vax_rate 
two_dose_rate / non_vax_rate
two_dose_rate / one_dose_rate


# Compare Hospitalization Rates

# From presentation
one_dose_hosps <- 141

# Estimate total hospitalizations from Situation Report (underestimate since don't have data from Dec. 27 to Jan. 16)
# Load BC Situation Report Data
setwd("~/GitHub/COVID19/2021-04_BC_Covid/Shiny_App")
df_situation <- readRDS("sitrep_data.rds") %>%
                mutate(date = as.Date(year_week, format="%d/%m/%Y"),
                       year_week = as.character(as.Date(year_week, format="%d/%m/%Y"))) %>%
                filter(!year_week %in% c("2020-05-07","2020-10-17","2021-01-09")) 


total_hosps_df <- df_situation %>%
                  filter(Age_Group %in% c('60-69','70-79','80-89','90+'),
                         metric == 'Hospitalizations',
                         year_week %in% c('2021-05-01','2021-01-16')) %>%
                  group_by(year_week) %>%
                  summarise(tot = sum(total))

total_hosps <- as.numeric(total_hosps_df[total_hosps_df$year_week == '2021-05-01','tot']) - as.numeric(total_hosps_df[total_hosps_df$year_week == '2021-01-16','tot'])

no_dose_hosps <- total_hosps - one_dose_hosps

non_vax_rate = (no_dose_hosps / total_non_vax_days) * 1000000
one_dose_rate = (one_dose_hosps / total_one_dose_vax_days) * 1000000

non_vax_rate
one_dose_rate

one_dose_rate / non_vax_rate 
