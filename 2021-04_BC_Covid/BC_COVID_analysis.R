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

area <- 'Vancouver Island'  # Select Region

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
          caption = "Chart: Michael Hainke Data: BCCDC Case Data Apr 29/21")
p


## COVID HOSP/ICU/DEATH HEATMAPS ##
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
          caption = "Chart: Michael Hainke Data: BC CDC Situation Report May 5/21")
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




