# Libraries
library(tidyverse)
library(lubridate)

#########################
## COVID CASE HEATMAPS ##
#########################

# Data:  http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Case_Details.csv

# Load BC CDC Case Data
df_case <- read.csv("BCCDC_COVID19_Dashboard_Case_Details.csv") %>%
           mutate(Reported_Date = as.Date(Reported_Date),
                  yr = year(as.Date(Reported_Date)),
                  mth = month(as.Date(Reported_Date)),
                  wk = week(as.Date(Reported_Date))) %>%
           mutate(year_week = as.character(ymd(paste0(yr,"-01-07")) + weeks(wk-1))) %>%
           filter(!wk %in% 53,
                  !Age_Group %in% c('Unknown'),
                  !HA %in% c('Out of Canada'))

# Load BC Population Data
df_pop <- read.csv("BC_Population_Demographics.csv")

# Summarize BC Weekly Case Data
df_summary_bc <- df_case %>%
                 group_by(year_week, Age_Group) %>%
                 summarise(cases = n()) %>%
                 mutate(HA = "BC") %>%
                 select(HA, year_week, Age_Group, cases)

# Summarize Weekly Case Data
df_summary_ha <- df_case %>%
                 group_by(HA, year_week, Age_Group) %>%
                 summarise(cases = n())

df_summary <- rbind(df_summary_bc, df_summary_ha) %>%
              ungroup() %>%
              complete(HA, year_week, Age_Group, fill = list(cases = 0)) %>%
              left_join(df_pop, by = c("Age_Group"="age_group", "HA"="HA")) %>%
              mutate(case_rate = cases / population * 100000) %>%
              mutate(case_rate_bin = cut(case_rate,
                                         c(-1,5,10,15,20,35,50,100,150,200,300,600,Inf),
                                         labels = c("0-5",">5-10",">10-15",">15-20",">20-35",">35-50",">50-100",">100-150",">150-200",">200-300",">300-600",">600")))

area <- 'BC'  # Select Region

# Case Heatmap 
p <- ggplot(filter(df_summary, HA == area), aes(year_week, Age_Group, fill= case_rate_bin)) + 
     geom_tile() +
     geom_text(size = 2, aes(label = round(case_rate, 0))) +
     scale_fill_brewer(type="seq",palette = "Spectral", direction = -1) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
     labs(fill = "cases per 100k population",
          y = "Age Group",
          x = "Week",
          title = paste0(area," Weekly COVID19 Cases per 100,000 Population"),
          caption = "Chart: Michael Hainke Data: BCCDC Case Data")
p

###################################
## COVID HOSP/ICU/DEATH HEATMAPS ##
###################################

## Data:  Manually entered from BC CDC situation reports  http://www.bccdc.ca/health-info/diseases-conditions/covid-19/data#Situationreport

# Load BC Situation Report Data
df_situation <- read.csv("BC_COVID_Weekly_Data.csv") %>%
                mutate(date = as.Date(year_week, format="%d/%m/%Y"),
                       year_week = as.character(as.Date(year_week, format="%d/%m/%Y"))) %>%
                filter(!year_week %in% c("2020-05-07","2020-10-17","2021-01-09")) 

measure <- "Hospitalizations"  # Select metric (Hospitalizations, ICU, Deaths)

df <- df_situation %>%
      filter(metric == measure) %>% 
      left_join(filter(df_pop, HA == "BC"), by = c("Age_Group"="age_group")) %>%
      mutate(rate = weekly_count / population * 1000000) %>%
      mutate(rate_bin = cut(rate,
                             c(-100,5,10,15,20,35,50,100,150,300,600,Inf),
                             labels = c("0-5",">5-10",">10-15",">15-20",">20-35",">35-50",">50-100",">100-150",">150-300",">300-600",">600")))

p <- ggplot(df, aes(year_week, Age_Group, fill= rate_bin)) + 
     geom_tile() +
     geom_text(size = 3, aes(label = round(rate, 0))) +
     scale_fill_brewer(type="seq",palette = "Spectral", direction = -1) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
     labs(fill = paste0(measure," per 1m age group population"),
          y = "Age Group",
          x = "Week",
          title = paste0("BC Weekly COVID19 ",measure," per 1,000,000 Age Group Population"),
          caption = "Chart: Michael Hainke Data: BC CDC Situation Report")
p

# Calculate Hospitalization Rate by Case (not pop)
# Assume 2 week lag from case to hospitalization

df_case <- df_situation %>%
           filter(metric == "Cases") %>%
           select(Age_Group, weekly_cases = weekly_count, date)

df <- df_situation %>%
      mutate(date_minus_14 = date - 14) %>%
      filter(metric == "Hospitalizations") %>%
      left_join(df_case, by = c("Age_Group"="Age_Group", "date_minus_14"="date")) %>%
      filter(!is.na(weekly_cases)) %>%
      mutate(rate = weekly_count / weekly_cases * 1000) %>%
      mutate(rate_bin = cut(rate,
                            c(-1000,50,100,200,300,400,500,600,700,800,900,Inf),
                            labels = c("0-50",">50-100",">100-200",">200-300",">300-400",">400-500",">500-600",">600-700",">700-800",">800-900",">900")))

p <- ggplot(df, aes(year_week, Age_Group, fill= rate_bin)) + 
     geom_tile() +
     geom_text(size = 3, aes(label = round(rate, 0))) +
     scale_fill_brewer(type="seq",palette = "Spectral", direction = -1) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
     labs(fill = "Hospitalized per 1000 cases",
          y = "Age Group",
          x = "Week",
          title = "Estimated Hospitalizations per 1000 cases (Hospitalizations / Cases 2 Weeks Prior)",
          caption = "Chart: Michael Hainke Data: BC CDC Situation Report Apr 28/21")
p

###########################
## LTC Outbreak Analysis ##
###########################

# Load LTC data
df_ltc <- read.csv("BC_COVID_LTC.csv") %>%
          mutate(outbreak_start_date = parse_date_time(outbreak_start_date, orders = c('mdy', 'ymd')),
                 outbreak_end_date = parse_date_time(outbreak_end_date, orders = c('mdy', 'ymd'))) %>%
          mutate(yr = year(as.Date(outbreak_start_date)),
                 wk = week(as.Date(outbreak_start_date))) %>%
          mutate(year_week = as.character(ymd(paste0(yr,"-01-07")) + weeks(wk-1)))

# Tidy LTC data
df_tidy <- rename(df_ltc, LTC_cases = cases_residents, LTC_deaths = deaths_residents, LTC_staff_cases = cases_staff, LTC_staff_deaths = deaths_staff) %>%
           pivot_longer(cols = c(LTC_cases, LTC_deaths, LTC_staff_cases, LTC_staff_deaths),
                        names_to = "metric",
                        values_to = "count") %>%
           group_by(metric, year_week) %>%
           summarise(cases = sum(count)) %>%
           select(metric, year_week, cases) %>%
           filter(metric %in% c("LTC_cases", "LTC_staff_cases"))

# Summarize BC Weekly Case Data
df_summary_bc <- df_case %>%
                 mutate(metric = case_when(Age_Group %in% c("70-79","80-89","90+") ~ "70+ Total",
                                           Age_Group %in% c("60-69") ~ "60-69",
                                           TRUE ~ "Total Under 60")) %>%
                 group_by(metric, year_week) %>%
                 summarise(cases = n()) %>%
                 select(metric, year_week, cases) %>%
                 filter(metric %in% c("70+ Total", "60-69"))

df_tidy <- rbind(df_tidy, df_summary_bc)

p <- ggplot(df_tidy, aes(x=year_week, y=cases, group=metric, colour=metric)) + 
   geom_line(size=1) +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
   labs(y = "Number of Cases",
        x = "Week",
        title = "BC Weekly COVID19 Cases",
        caption = "Chart: Michael Hainke  Data: BC CDC")
p

################################
## Vaccine Efficacy Estimates ##
################################

# Breakthrough Cases (from BC CDC presentation)
one_dose_cases = 1340
two_dose_cases = 120
non_vaccinated_cases = 78020

# Alternate: Calculate only > 60 Adult Cases from BC CDC Case Data (to approximate similar demographics to vaccinated)
total_cases <- df_case %>%
               filter(!Age_Group %in% c('<10', '10-19', '20-39', '30-39', '40-49', '50-59'),
                      Reported_Date %within% interval(ymd("2020-12-27"), ymd("2021-05-01"))) %>%
               count()
non_vaccinated_cases <- total_cases - one_dose_cases - two_dose_cases

# Load Covid Tracker data (to get vaccination dates)
# Data: https://covid19tracker.ca/vaccinationtracker.html

df_vax <- read.csv("COVID19Tracker.ca Data - BC.csv") %>%
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

