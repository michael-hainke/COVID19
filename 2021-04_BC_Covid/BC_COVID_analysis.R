# Libraries
library(tidyverse)
library(lubridate)

## COVID CASE HEATMAPS ##
## Data:  http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Dashboard_Case_Details.csv

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
          title = paste0(area," Weekly COVID19 Cases per 100,000 Population"))
p


## COVID HOSP/ICU/DEATH HEATMAPS ##
## Data:  Manually entered from BC CDC situation reports  http://www.bccdc.ca/health-info/diseases-conditions/covid-19/data#Situationreport

# Load BC Situation Report Data
df_situation <- read.csv("BC_COVID_Weekly_Data.csv") %>%
                mutate(year_week = as.character(as.Date(year_week, format="%d/%m/%Y"))) %>%
                filter(!year_week %in% c("2020-05-07","2020-10-17","2021-01-09")) 

measure <- "Deaths"  # Select metric (Hospitalizations, ICU, Deaths)

df <- df_situation %>%
      filter(metric == measure) %>% 
      left_join(filter(df_pop, HA == "BC"), by = c("Age_Group"="age_group")) %>%
      mutate(rate = weekly_count / population * 1000000) %>%
      mutate(rate_bin = cut(rate,
                             c(-100,5,10,15,20,35,50,100,150,300,600,Inf),
                             labels = c("0-5",">5-10",">10-15",">15-20",">20-35",">35-50",">50-100",">100-150",">150-300",">300-600",">600")))

p <- ggplot(df, aes(year_week, Age_Group, fill= rate_bin)) + 
     geom_tile() +
     geom_text(size = 2, aes(label = round(rate, 0))) +
     scale_fill_brewer(type="seq",palette = "Spectral", direction = -1) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="bottom") + 
     labs(fill = paste0(measure," per 1m population"),
          y = "Age Group",
          x = "Week",
          title = paste0("BC Weekly COVID19 ",measure," per 1,000,000 Population"))
p
